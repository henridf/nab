(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc

type diffusion_type = [ `Voronoi | `OPP | `ESS ]

let diffusion_type:(diffusion_type ref) = ref `Voronoi

let strset_difftype s = match s with 
  | "Voronoi" | "voronoi" | "vor" | "Vor" -> diffusion_type := `Voronoi
  | "OPP" | "opp" -> diffusion_type := `OPP
  | "ESS" | "ess" -> diffusion_type := `ESS
  | _ -> raise (Failure ("Invalid difftype "^s))

let mean_interest_interval = ref 60.
let interest_lambda() = 1. /.  !mean_interest_interval

let nsinks_ = ref None
let nsinks() = o2v !nsinks_

let rndseed = ref 0 

class type diff_agent_t =
  object
    inherit Log.inheritable_loggable
    inherit Rt_agent.t

    method app_recv_l4pkt : L4pkt.t -> Common.nodeid_t ->  unit
      (* Publish must take a ~dst because of the rt_agent_base.t interface.
	 But in fact, since we are
	 data-centric, we don't do anything with it *)

    method seqno : unit -> int
    method is_closest_sink : ?op:(int -> int -> bool) -> Common.nodeid_t -> bool
    method subscribe : ?delay:float ->  ?one_shot:bool -> ?ttl:int -> unit -> unit
    method unsubscribe : unit
    method get_rtab : Rtab.t

    method closest_sinks : unit -> Common.nodeid_t list 
      (** Returns closest sink (or sinks, if more than one at closest
	distance). If this node itself is a sink, it is not returned, and the
	distance 0 to itself is not considered *)

    method known_sinks : unit -> Common.nodeid_t list
      (** Returns all sinks to whom this node has a gradient (except this node
	itself, if it is a sink*)
  end


exception Send_Out_Failure

let agents_array = ref ([||]:diff_agent_t array)

let set_agents arr = agents_array := arr
let agent i = !agents_array.(i)


let _ERS_START_TTL = 2
let _ERS_MULT_FACT = 2


class diff_agent  ?(stack=0) theowner : diff_agent_t = 
object(s)

  inherit Log.inheritable_loggable
  inherit Rt_agent_base.base ~stack theowner 

  val rt = Rtab.create_grep ~size:(Param.get Params.nodes) 
  val mutable seqno = 0
  val mutable subscribed = false
  val pktqs = Array.init (Param.get Params.nodes) (fun n -> Queue.create()) 
  val rnd = Random.State.make [|!rndseed|]

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable) "/diffagent";
    s#incr_seqno();
    incr rndseed;
  )

  method myid = myid

  method get_rtab = rt

  method seqno() = seqno

  method private incr_seqno() = (
    seqno <- seqno + 1;
    let update = 
      Rtab.newadv 
	~rt 
	~dst:myid
	~sn:seqno
	~hc:0
	~nh:myid
    in 
    assert(update);
  )

  (* wrapper around Rtab.newadv which additionally checks for 
     open rreqs to that dest and cancels if any,
     buffered packets to that dest and sends them if any *)
  method private newadv ~dst ~sn ~hc ~nh  = (
      let update = 
	Rtab.newadv ~rt ~dst ~sn ~hc ~nh
      in
      if update then (
	s#log_info 
	(lazy (sprintf "New route to dst %d: nexthop %d, hopcount %d, seqno %d"
	  dst nh hc sn));
	Rtab.repair_done ~rt ~dst;
	(* if route to dst was accepted, send any packets that were waiting
	   for a route to this dst *)
      );
      update
    )

  method private packet_fresh ~l3pkt = (
    let diff_hdr = L3pkt.diff_hdr l3pkt in
    let pkt_ssn = Diff_pkt.ssn diff_hdr in
    match (Rtab.seqno ~rt ~dst:(L3pkt.l3src l3pkt)) with
      | None -> true 
      | Some s when (pkt_ssn > s) -> true
      | Some s when (pkt_ssn = s) -> 
	  Diff_pkt.shc diff_hdr 
	  <
	  o2v (Rtab.hopcount ~rt ~dst:(L3pkt.l3src l3pkt))
      | Some s when (pkt_ssn < s) -> false
      | _ -> raise (Misc.Impossible_Case "Diff_agent.packet_fresh()")
  )
    
  method mac_recv_l3pkt _ = ()
  method mac_recv_l2pkt l2pkt = (
    
    let l3pkt = L2pkt.l3pkt ~l2pkt:l2pkt in
    let diff_hdr = L3pkt.diff_hdr l3pkt in

    assert (L3pkt.l3ttl ~l3pkt >= 0);

    let l2sender = L2pkt.l2src l2pkt in

    begin match Diff_pkt.flags diff_hdr with
      | Diff_pkt.DIFF_DATA -> s#process_data_pkt ~l3pkt;
      | Diff_pkt.DIFF_RADV -> s#process_radv_pkt ~l3pkt ~l2sender 
    end
  )

  method known_sinks () = (
    let sinks = ref [] in
    for i = 0 to (Param.get Params.nodes - 1) do 
      match (Rtab.hopcount ~rt ~dst:i) with
	| None -> ()
	| Some hc -> sinks := i::!sinks
    done;
    let sinks = 
      List.filter (fun i -> i <> myid) !sinks
    in if ((List.length sinks) > nsinks()) then
      s#log_error (lazy (sprintf "nsinks %d, sinks %s" (nsinks()) (Misc.sprintlist "%d" sinks)));
    
    sinks
  )
 
  method closest_sinks () = (
    let sinks = s#known_sinks() in
    let closest_hops = 
    List.fold_left 
      (fun closest sink -> min closest (o2v (Rtab.hopcount ~rt ~dst:sink)))
      max_int 
      sinks
    in
    if closest_hops = max_int then []
    else 
      List.filter (fun sink -> 
	(o2v (Rtab.hopcount ~rt ~dst:sink)) = closest_hops) sinks
  )


  method private is_closest_sink ?(op=(>=)) sink = 
    if Rtab.hopcount ~rt ~dst:sink = None then 
      raise (Failure "Diff_agent.is_closest_sink: called for a sink which was not in our
    routing table");
    
    (s#closest_sinks ()) = [sink] || ((s#closest_sinks()) = [])
    (* this one is equivalent to block below with op=(>) *)
(*    (List.mem sink (s#closest_sinks())) ||  ((s#closest_sinks()) = [])
    (* this one is equivalent to block below with op=(>=) *)

    let d_to_sink s = 
      match (Rtab.hopcount ~rt ~dst:s) with
	| None -> max_int
	| Some hc -> hc 
    in
    List.fold_left 
      (fun bool othersink ->
	bool &&
	(op (d_to_sink othersink) ((d_to_sink sink) )))
      true
      ((List.filter (fun s -> s <> sink)) (s#known_sinks()))
*)


  method private process_radv_pkt ~l3pkt ~l2sender = (
    (* update route to source if packet came over fresher route than what we
       have *)

    let diff_hdr = L3pkt.diff_hdr l3pkt in

    let pkt_fresh = (s#packet_fresh ~l3pkt)
    and update =  
      s#newadv 
	~dst:(L3pkt.l3src ~l3pkt)
	~sn:(Diff_pkt.ssn diff_hdr)
	~hc:(Diff_pkt.shc diff_hdr)
	~nh:l2sender
    in
    assert (update = pkt_fresh);
    
    s#log_info 
      (lazy (sprintf "Received Interest pkt from src %d "
	(L3pkt.l3src ~l3pkt) ));
    let sink_closest = (s#is_closest_sink (L3pkt.l3src ~l3pkt)) in
    match pkt_fresh, sink_closest, !diffusion_type with 
      | false, _, (_:diffusion_type) ->
 	  s#log_info   (lazy (sprintf "Dropping Interest pkt from src %d (not fresh)"
	    (L3pkt.l3src ~l3pkt) ))
      | true,true, _ 
      | true, false, `OPP 
      | true, false, `ESS  ->  
	  s#send_out ~l3pkt
      | true, false, `Voronoi -> 
 	  s#log_info   (lazy (sprintf "Dropping Interest pkt from src %d (not closest sink)"
	    (L3pkt.l3src ~l3pkt)))
  ) 


  method private process_data_pkt 
    ~(l3pkt:L3pkt.t) =  (
      
      let dst = (L3pkt.l3dst ~l3pkt) in
      begin try
	
	if (dst = myid) then ( (* pkt for us *)
	  s#hand_upper_layer ~l3pkt;
	  raise Break 
	);
	
	if  (Rtab.repairing ~rt ~dst)  then (
	  raise Break
	);		

	begin try 
	  s#send_out ~l3pkt
	with 
	  | Send_Out_Failure -> 
	      begin
		s#log_notice 
		  (lazy (sprintf "Forwarding DATA pkt to dst %d failed, dropping."
		    dst));
		raise (Failure "diffusion not implemented fully");
	      end
	end
      with 
	| Break -> ()
	| e -> raise e;
      end;
      ()
    )


  method private send_out ~l3pkt = (
    let newpkt = L3pkt.clone_l3pkt ~l3pkt in
    let diff_hdr = L3pkt.diff_hdr l3pkt in

    let l3pkt = 1 in
    let dst = L3pkt.l3dst ~l3pkt:newpkt in
    assert (dst <> myid);
    assert (L3pkt.l3ttl ~l3pkt:newpkt >= 0);
    assert (Diff_pkt.ssn diff_hdr >= 1);

    let failed() = (
      Diff_pkt.decr_shc_pkt diff_hdr;
      raise Send_Out_Failure
    ) in
    s#incr_seqno();
    Diff_pkt.incr_shc_pkt diff_hdr;
    assert (Diff_pkt.shc diff_hdr > 0);
    begin match (Diff_pkt.flags diff_hdr) with
      | Diff_pkt.DIFF_RADV -> 
	  assert (dst = L3pkt._L3_BCAST_ADDR);
	  L3pkt.decr_l3ttl ~l3pkt:newpkt;
	  begin match ((L3pkt.l3ttl ~l3pkt:newpkt) >= 0)  with
	    | true -> 
		s#mac_bcast_pkt newpkt;
	    | false ->
		s#log_info (lazy (sprintf "Dropping packet (negative ttl)"));		
	  end
      | Diff_pkt.DIFF_DATA ->
	  begin 
	    let nexthop = 
	      match Rtab.nexthop ~rt ~dst  with
		| None -> failed()
		| Some nh -> nh 
	    in 
	    try begin
	      s#mac_send_pkt ~dstid:nexthop newpkt; end
	    with Simplenode.Mac_Send_Failure -> failed()
	      
	  end
    end
  )

    
  (* this is a null method because so far we don't need to model apps getting
     packets since we model CBR streams, and mhook catches packets as they enter
     the node *)
  method private hand_upper_layer ~l3pkt = (
     s#log_info (lazy (sprintf "Received app pkt from src %d"
	  (L3pkt.l3src ~l3pkt)));
  )

  method private send_interest ?(one_shot=false) ?(ttl=255) () = (
    s#log_notice (lazy "Sending interest");
    let diff_hdr = (Diff_pkt.make_diff_hdr
      ~flags:Diff_pkt.DIFF_RADV ~ssn:seqno ~shc:0) in
    if subscribed then 
      begin
	let l3hdr = L3pkt.make_l3hdr
	  ~srcid:myid	~dstid:L3pkt._L3_BCAST_ADDR ~ttl
	  ~ext:(`DIFF_HDR diff_hdr) () in

	let l3pkt = (L3pkt.make_l3pkt ~l3hdr ~l4pkt:`APP_PKT) in

	s#send_out ~l3pkt;

	if (not one_shot) then begin
	  let next_interest_timeout = expo ~rand:(Random.State.float rnd 1.0)
	    ~lambda:(interest_lambda()) in
	  (Sched.s())#sched_in ~f:(s#send_interest ~ttl)
	  ~t:next_interest_timeout 
	end
      end;

      if one_shot then s#unsubscribe
  )



  method unsubscribe = 
    if not subscribed then
      s#log_error (lazy "Not subscribed");
    subscribed <- false;
    s#log_notice (lazy "Unsubscribing")

  method subscribe ?delay ?(one_shot=false) ?(ttl=255) () = (
    if subscribed then 
      s#log_error (lazy "Already Subscribed");
    subscribed <- true;
    s#log_notice (lazy "Subscribing");
    let f = s#send_interest ~one_shot ~ttl in
    match delay with 
      | None -> f ();
      | Some t -> (Sched.s())#sched_in ~f ~t
  )

  method private send_data_to_sink sink = 
      s#log_info (lazy (sprintf "Originating data pkt to sink %d"
	sink));
    let diff_hdr = (Diff_pkt.make_diff_hdr   ~flags:Diff_pkt.DIFF_DATA
      ~ssn:seqno   ~shc:0) in
      let l3hdr = L3pkt.make_l3hdr  ~srcid:myid ~dstid:sink  ~ttl:255
	~ext:(`DIFF_HDR diff_hdr)  ()
      in
      let l3pkt = (L3pkt.make_l3pkt ~l3hdr:l3hdr ~l4pkt:`APP_PKT) in
      s#send_out ~l3pkt;


  method app_recv_l4pkt _ _ = 
    s#publish

  method private publish  = (
    let sinks_to_send_to = 
      match !diffusion_type with 
	| `Voronoi | `ESS -> 
	    let candidates = s#closest_sinks () in 
	    if candidates = [] then [] else
	      [Misc.rnd_from_list candidates]
	| `OPP -> 
	    s#known_sinks()
    in
    (* Jitter the sends by a uniform RV over a range proportional to the
       number of sends *)
    List.iter (fun sink -> 
      (Sched.s())#sched_in ~f:(fun _ -> s#send_data_to_sink sink)
      ~t:(Random.State.float rnd (float (List.length sinks_to_send_to))))
      sinks_to_send_to

  )




end
