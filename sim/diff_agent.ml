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
    method publish :  L4pkt.l4pkt_t -> dst:Common.nodeid_t ->  unit
      (* Publish must take a ~dst because it is passed to
	 Simplenode.add_app_send_pkt_hook - in fact, since we are
	 data-centric, we don't do anything with it *)
    method seqno : unit -> int
    method  is_closest_sink : ?op:(int -> int -> bool) -> Common.nodeid_t -> bool
    method subscribe : ?delay:float -> ?ttl:int -> unit -> unit
    method private hand_upper_layer : l3pkt:L3pkt.t -> unit
    method private incr_seqno : unit -> unit
    method get_rtab : Rtab.rtab_t
    method newadv : 
      dst:Common.nodeid_t -> 
      sn:int -> hc:int -> nh:int ->
      bool
    method objdescr : string
    method private packet_fresh : l3pkt:L3pkt.t -> bool
    method private process_data_pkt : l3pkt:L3pkt.t -> unit
    method private process_radv_pkt :
      l3pkt:L3pkt.t -> 
      l2sender:Common.nodeid_t -> unit
    method private recv_l2pkt_hook : L2pkt.t -> unit
    method private send_out : l3pkt:L3pkt.t -> unit

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


class diff_agent owner : diff_agent_t = 
object(s)

  inherit Log.inheritable_loggable

  val owner:Simplenode.simplenode = owner
  val rt = Rtab.create_grep ~size:(Param.get Params.nodes) 
  val mutable seqno = 0
  val mutable subscribed = false
  val pktqs = Array.init (Param.get Params.nodes) (fun n -> Queue.create()) 
  val rnd = Random.State.make [|!rndseed|]

  initializer (
    s#set_objdescr ~owner "/diffagent";
    owner#add_recv_l2pkt_hook ~hook:s#recv_l2pkt_hook;
    owner#add_app_send_pkt_hook ~hook:s#publish;
    s#incr_seqno();
    incr rndseed;
  )

  method get_rtab = rt

  method seqno() = seqno

  method private incr_seqno() = (
    seqno <- seqno + 1;
    let update = 
      Rtab.newadv 
	~rt 
	~dst:owner#id
	~sn:seqno
	~hc:0
	~nh:owner#id
    in 
    assert(update);
  )

  (* wrapper around Rtab.newadv which additionally checks for 
     open rreqs to that dest and cancels if any,
     buffered packets to that dest and sends them if any *)
  method newadv ~dst ~sn ~hc ~nh  = (
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
    let pkt_ssn = L3pkt.ssn ~l3pkt in
    match (Rtab.seqno ~rt ~dst:(L3pkt.l3src l3pkt)) with
      | None -> true 
      | Some s when (pkt_ssn > s) -> true
      | Some s when (pkt_ssn = s) -> 
	  L3pkt.shc l3pkt 
	  <
	  o2v (Rtab.hopcount ~rt ~dst:(L3pkt.l3src l3pkt))
      | Some s when (pkt_ssn < s) -> false
      | _ -> raise (Misc.Impossible_Case "Grep_agent.packet_fresh()")
  )
    
  method private recv_l2pkt_hook l2pkt = (
    
    let l3pkt = L2pkt.l3pkt ~l2pkt:l2pkt in
    assert (L3pkt.l3ttl ~l3pkt >= 0);

    let l2sender = L2pkt.l2src l2pkt in

    begin match L3pkt.l3grepflags ~l3pkt with
      | L3pkt.GREP_DATA -> s#process_data_pkt ~l3pkt;
      | L3pkt.GREP_RADV -> s#process_radv_pkt ~l3pkt ~l2sender 
      | L3pkt.GREP_RREP | L3pkt.GREP_RREQ 
      | L3pkt.NOT_GREP  | L3pkt.EASE 
	-> raise (Failure "Grep_agent.recv_l2pkt_hook");
      | L3pkt.GREP_RERR -> raise (Failure "Grep_agent.recv_l2pkt_hook");
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
      List.filter (fun i -> i <> owner#id) !sinks
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

    let pkt_fresh = (s#packet_fresh ~l3pkt)
    and update =  
      s#newadv 
	~dst:(L3pkt.l3src ~l3pkt)
	~sn:(L3pkt.ssn ~l3pkt)
	~hc:(L3pkt.shc ~l3pkt)
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
	    (L3pkt.l3src ~l3pkt) ))
  ) 


  method private process_data_pkt 
    ~(l3pkt:L3pkt.t) =  (
      
      let dst = (L3pkt.l3dst ~l3pkt) in
      begin try
	
	if (dst = owner#id) then ( (* pkt for us *)
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
		s#init_rreq  ~dst;
	      end
	end
      with 
	| Break -> ()
	| e -> raise e;
      end;
      ()
    )


  method private init_rreq ~dst = (
    (* for the initial route request, we don't do it if there's already one
       going on for this destination (which isn't really expected to happen,
       but you never know *)
    if (not (Rtab.repairing ~rt ~dst)) then (
      Rtab.repair_start ~rt ~dst;
      s#log_info 
      (lazy (sprintf "Initializing RREQ for dst %d" dst ));
      s#send_rreq ~ttl:_ERS_START_TTL ~dst
    )
  )

  method private send_rreq  ~ttl ~dst = (
    
    if (Rtab.repairing ~rt ~dst) then (
      s#log_info (lazy (sprintf "Sending RREQ pkt for dst %d with ttl %d"
	dst ttl));
      
      let (dseqno,dhopcount) = 
	begin match (Rtab.seqno ~rt ~dst) with
	  | None -> (0, max_int)
	  | Some s -> (s, o2v (Rtab.hopcount ~rt ~dst)) end
      in
      let grep_l3hdr_ext = 
	L3pkt.make_grep_l3hdr_ext
	  ~flags:L3pkt.GREP_RREQ
	  ~ssn:seqno
	  ~shc:0
	  ~rdst:dst
	  ~dsn:dseqno
	  ~dhc:dhopcount
	  ()
      in
      let l3hdr = 
	L3pkt.make_l3hdr
	  ~srcid:owner#id
	  ~dstid:L3pkt._L3_BCAST_ADDR
	  ~ext:grep_l3hdr_ext
	  ~ttl:ttl 
	  ()
      in
      let l3pkt = 
	L3pkt.make_l3pkt ~l3hdr ~l4pkt:`NONE
      in
      let next_rreq_ttl = 
	(ttl*_ERS_MULT_FACT) in
      let next_rreq_timeout = 
	((i2f next_rreq_ttl) *. 0.02) in
      let next_rreq_event() = 
	  (s#send_rreq 
	    ~ttl:next_rreq_ttl
	    ~dst)
      in	

	s#send_out ~l3pkt;
	(* we say that maximum 1-hop traversal is 20ms, 
	   ie half of value used by AODV. Another difference relative to AODV
	   is that we use ttl, not (ttl + 2).
	   This is ok while we use a simple MAC, and ok since our AODV impl 
	   will use the same values*)
	
	
(*	if next_rreq_ttl < ((Param.get Params.nodes)/10) then*)
	  (Gsched.sched())#sched_in ~f:next_rreq_event ~t:next_rreq_timeout;
    )
  )
    
    
  method private send_out ~l3pkt = (
    let newpkt = L3pkt.clone_l3pkt ~l3pkt in
    let l3pkt = 1 in
    let dst = L3pkt.l3dst ~l3pkt:newpkt in
    assert (dst <> owner#id);
    assert (L3pkt.l3ttl ~l3pkt:newpkt >= 0);
    assert (L3pkt.ssn ~l3pkt:newpkt >= 1);

    let failed() = (
      L3pkt.decr_shc_pkt ~l3pkt:newpkt;
      raise Send_Out_Failure
    ) in
    s#incr_seqno();
    L3pkt.incr_shc_pkt ~l3pkt:newpkt;
    assert (L3pkt.shc ~l3pkt:newpkt > 0);
    begin match (L3pkt.l3grepflags ~l3pkt:newpkt) with
      | L3pkt.GREP_RADV -> 
	  assert (dst = L3pkt._L3_BCAST_ADDR);
	  L3pkt.decr_l3ttl ~l3pkt:newpkt;
	  begin match ((L3pkt.l3ttl ~l3pkt:newpkt) >= 0)  with
	    | true -> 
		owner#mac_bcast_pkt ~l3pkt:newpkt;
	    | false ->
		s#log_info (lazy (sprintf "Dropping packet (negative ttl)"));		
	  end
      | L3pkt.GREP_DATA ->
	  begin 
	    let nexthop = 
	      match Rtab.nexthop ~rt ~dst  with
		| None -> failed()
		| Some nh -> nh 
	    in 
	    try begin
	      owner#mac_send_pkt ~l3pkt:newpkt ~dstid:nexthop; end
	    with Simplenode.Mac_Send_Failure -> failed()
	      
	  end
      | _ ->
	  raise (Failure "Grep_agent.send_out: unexpected packet type")
    end
  )
		
	

  (* this is a null method because so far we don't need to model apps getting
     packets since we model CBR streams, and mhook catches packets as they enter
     the node *)
  method private hand_upper_layer ~l3pkt = (
     s#log_info (lazy (sprintf "Received app pkt from src %d"
	  (L3pkt.l3src ~l3pkt)));
  )

  method private send_interest ?(ttl=255) () = (
    s#log_notice (lazy "Sending interest");

    let l3hdr = L3pkt.make_l3hdr
	~srcid:owner#id	~dstid:L3pkt._L3_BCAST_ADDR ~ttl
	~ext:(L3pkt.make_grep_l3hdr_ext
	  ~flags:L3pkt.GREP_RADV ~ssn:seqno ~shc:0 ()) () in

    let l3pkt = (L3pkt.make_l3pkt ~l3hdr:l3hdr ~l4pkt:`APP_PKT) in

    s#send_out ~l3pkt;

    let next_interest_timeout = expo ~rand:(Random.State.float rnd 1.0)
      ~lambda:(interest_lambda()) in
      (Gsched.sched())#sched_in ~f:s#send_interest ~t:next_interest_timeout
  )



  method subscribe ?delay ?(ttl=255) () = (
    if subscribed then 
      s#log_error (lazy "Already Subscribed");
    subscribed <- true;
    s#log_notice (lazy "Subscribing");
    match delay with 
      | None -> s#send_interest ();
      | Some t -> (Gsched.sched())#sched_in ~f:s#send_interest ~t
  )


  method private send_data_to_sink sink = 
      s#log_info (lazy (sprintf "Originating data pkt to sink %d"
	sink));
      let l3hdr = L3pkt.make_l3hdr  ~srcid:owner#id ~dstid:sink  ~ttl:255
	~ext:(L3pkt.make_grep_l3hdr_ext   ~flags:L3pkt.GREP_DATA
	  ~ssn:seqno   ~shc:0  ())  ()
      in
      let l3pkt = (L3pkt.make_l3pkt ~l3hdr:l3hdr ~l4pkt:`APP_PKT) in
      s#send_out ~l3pkt;


  method publish  _ ~dst:_  = (
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
      (Gsched.sched())#sched_in ~f:(fun _ -> s#send_data_to_sink sink)
      ~t:(Random.State.float rnd (float (List.length sinks_to_send_to))))
      sinks_to_send_to

  )




end
