(* we should have a 'valid neighbors' table. 
   once we implement that, then we do all the things like

   - do we invalidate the entry when a rrep forwarding fails??

   - if a nexthop is "gone" then all other routes which have him as neighbor
   should be invalidated (this may be what aodv does)
*)


(* can't there be races or other unexpected interactions when newadv
   immediately sends buffered packets? maybe safer to do this after haveing
   processed teh incoming packets (and also it would be more 'correct' since 
   the currently processed packet shouldn't get preempted by this one) *)

(* RTR 4 not implemented (updating entry to destination node) *)

(* maybe nodes not incrementing seqno on sending rrep??
17.331472 /node/84 /GREP_Agent this:84, nexthoph:300, dst:300, this_sn: 202, this_hc: 1, next_sn: 200, next_hc:
    0
Fatal error: exception Failure("Inv_packet_upwards this:84, nexthoph:300, dst:300, this_sn: 202, this_hc: 1, next_$    0")
*)
(*

  should we set the seqno on a data packet when ours is fresher???



  PPR5: is it a good idea to discard a packet when the buffer is full without
  initiating a RREQ? Maybe think we should initiate one (if there is no
  pkt for this destination in the buffer), since pkts will keep flowing our
  way.

  ttl is getting increased beyond limit
*)

(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Aodv_grep_common
open Printf
open Misc

let packet_buffer_size = 50

class type grep_agent_t =
  object
    method private app_send : L4pkt.l4pkt_t -> dst:Common.nodeid_t -> unit
    method private buffer_packet : l3pkt:L3pkt.l3packet_t -> unit
    method private hand_upper_layer : l3pkt:L3pkt.l3packet_t -> unit
    method private incr_seqno : unit -> unit
    method private inv_packet_upwards :
      nexthop:Common.nodeid_t -> l3pkt:L3pkt.l3packet_t -> unit
    method get_rtab : Rtab.rtab_t
    method newadv : 
      dst:Common.nodeid_t -> 
      sn:int -> hc:int -> nh:int ->
      bool
    method objdescr : string
    method private packet_fresh : l3pkt:L3pkt.l3packet_t -> bool
    method private queue_size : unit -> int
    method private packets_waiting : dst:Common.nodeid_t -> bool
    method private process_data_pkt : l3pkt:L3pkt.l3packet_t -> unit
    method private process_radv_pkt :
      l3pkt:L3pkt.l3packet_t -> 
      sender:Common.nodeid_t -> unit
    method private process_rrep_pkt :
      l3pkt:L3pkt.l3packet_t -> 
      sender:Common.nodeid_t -> 
      fresh:bool ->
      unit
    method private process_rreq_pkt :
      l3pkt:L3pkt.l3packet_t -> 
      fresh:bool -> unit
    method private recv_l2pkt_hook : L2pkt.l2packet_t -> unit
    method private send_out : l3pkt:L3pkt.l3packet_t -> unit
    method private send_rrep : dst:Common.nodeid_t -> obo:Common.nodeid_t -> unit
    method private send_rreq :
      ttl:int -> dst:Common.nodeid_t -> rreq_uid:int -> unit
    method private send_waiting_packets : dst:Common.nodeid_t -> unit
  end


exception Send_Out_Failure


let agents_array = ref ([||]:grep_agent_t array)

let set_agents arr = agents_array := arr
let agent i = !agents_array.(i)



class grep_agent owner : grep_agent_t = 
object(s)

  inherit Log.inheritable_loggable

  val owner:Simplenode.simplenode = owner
  val rt = Rtab.create_grep ~size:(Param.get Params.nodes) 
  val mutable seqno = 0
  val pktqs = Array.init (Param.get Params.nodes) (fun n -> Queue.create()) 

  val rreq_uids = Array.create (Param.get Params.nodes) 0
    (* see #init_rreq for explanation on this *)

  initializer (
    s#set_objdescr ~owner "/GREP_Agent";
    owner#add_recv_l2pkt_hook ~hook:s#recv_l2pkt_hook;
    owner#add_app_send_pkt_hook ~hook:s#app_send;
    s#incr_seqno()
  )

  method get_rtab = rt

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

  method private packets_waiting ~dst = 
    not (Queue.is_empty pktqs.(dst))

  method private queue_size() = 
    Array.fold_left (fun n q -> n + (Queue.length q))  0 pktqs

  method private send_waiting_packets ~dst = 
    while s#packets_waiting ~dst do
      let pkt = (Queue.pop pktqs.(dst)) in
      try 
	s#log_info 
	  (lazy (sprintf "Sending buffered DATA pkt from src %d to dst %d."
	    (L3pkt.l3src ~l3pkt:pkt) dst));
	s#send_out ~l3pkt:pkt
      with 
	| Send_Out_Failure -> 
	    s#log_warning 
	    (lazy (sprintf "Sending buffered DATA pkt from src %d to dst %d failed, dropping"
	      (L3pkt.l3src ~l3pkt:pkt) dst));
    done
    
  (* DATA packets are buffered when they fail on send, 
     or if there are already buffered packets for that destination *)
  method private buffer_packet ~(l3pkt:L3pkt.l3packet_t) = (
    match s#queue_size() < packet_buffer_size with 
      | true ->
	  let dst = L3pkt.l3dst ~l3pkt in
	  assert (dst <> L3pkt._L3_BCAST_ADDR);
	  Queue.push l3pkt pktqs.(dst);
      | false -> (
	  Grep_hooks.drop_data();
	  s#log_notice (lazy (sprintf "Dropped packet for dst %d" 
	    (L3pkt.l3dst ~l3pkt)))
	)
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
	if (s#packets_waiting ~dst) then (
	  s#send_waiting_packets ~dst
	);

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
    (* create or update 1-hop route to previous hop *)
    let sender = L2pkt.l2src l2pkt in
    if (sender <> (L3pkt.l3src ~l3pkt)) then (
      let sender_seqno = 
	match Rtab.seqno ~rt ~dst:sender with
	  | None -> 1
	  | Some n -> n + 1
      in
      let update =  
	s#newadv 
	  ~dst:sender
	  ~sn:sender_seqno
	  ~hc:1
	  ~nh:sender
      in
      assert (update);
    );
    (* update route to source if packet came over fresher route than what we
       have *)
    let pkt_fresh = (s#packet_fresh ~l3pkt)
    and update =  
      s#newadv 
	~dst:(L3pkt.l3src ~l3pkt)
	~sn:(L3pkt.ssn ~l3pkt)
	~hc:(L3pkt.shc ~l3pkt)
	~nh:sender
    in
    assert (update = pkt_fresh);
    
    (* hand off to per-type method private *)
    begin match L3pkt.l3grepflags ~l3pkt with
      | L3pkt.GREP_DATA -> s#process_data_pkt ~l3pkt;
      | L3pkt.GREP_RREQ -> s#process_rreq_pkt ~l3pkt ~fresh:pkt_fresh
      | L3pkt.GREP_RADV -> s#process_radv_pkt ~l3pkt ~sender;
      | L3pkt.GREP_RREP -> s#process_rrep_pkt ~l3pkt ~sender ~fresh:pkt_fresh;
      | L3pkt.NOT_GREP | L3pkt.EASE 
	-> raise (Failure "Grep_agent.recv_l2pkt_hook");
      | L3pkt.GREP_RERR -> raise (Failure "Grep_agent.recv_l2pkt_hook");
    end
  )

  method private process_radv_pkt ~l3pkt ~sender = 
    raise Misc.Not_Implemented

  method private process_rreq_pkt ~l3pkt ~fresh = (
    
    let rdst = (L3pkt.rdst ~l3pkt) 
    and dsn =  (L3pkt.dsn ~l3pkt) 
    in
    s#log_info 
      (lazy (sprintf "Received RREQ pkt from src %d for dst %d"
	(L3pkt.l3src ~l3pkt) rdst));
    match fresh with 
      | true -> 
	  let answer_rreq = 
	    (rdst = owner#id)
	    ||
	    begin match (Rtab.seqno ~rt ~dst:rdst) with 
	      | None -> false
	      | Some s when (s > dsn) -> true
	      | Some s when (s = dsn) ->
		  (o2v (Rtab.hopcount ~rt ~dst:rdst)
		  <
		  (L3pkt.dhc ~l3pkt) + L3pkt.shc ~l3pkt)
	      | Some s when (s < dsn) -> false
	      | _ -> raise (Misc.Impossible_Case "Grep_agent.answer_rreq()") end
	  in
	  if (answer_rreq) then 
	    s#send_rrep 
	      ~dst:(L3pkt.l3src ~l3pkt)
	      ~obo:rdst
	  else (* broadcast the rreq further along *)
	    s#send_out ~l3pkt
      | false -> 
	  s#log_info 
	  (lazy (sprintf "Dropping RREQ pkt from src %d for dst %d (not fresh)"
	    (L3pkt.l3src ~l3pkt) rdst));
  )
      
  method private send_rrep ~dst ~obo = (
    s#log_info 
    (lazy (sprintf "Sending RREP pkt to dst %d, obo %d"
      dst obo));
    let grep_l3hdr_ext = 
      L3pkt.make_grep_l3hdr_ext 
	~flags:L3pkt.GREP_RREP
	~ssn:seqno
	~shc:0
	~osrc:obo
	~osn:(o2v (Rtab.seqno ~rt ~dst:obo))
	~ohc:(o2v (Rtab.hopcount ~rt ~dst:obo))
	()
    in
    let l3hdr = 
      L3pkt.make_l3hdr
	~srcid:owner#id
	~dstid:dst
	~ext:grep_l3hdr_ext
	()
    in
    let l3pkt =
      L3pkt.make_l3pkt ~l3hdr ~l4pkt:`NONE
    in
    try 
      s#send_out  ~l3pkt
    with 
      | Send_Out_Failure -> 
	  s#log_notice 
	  (lazy (sprintf "Sending RREP pkt to dst %d, obo %d failed, dropping"
	    dst obo));
  )

  method private inv_packet_upwards ~nexthop ~l3pkt = (
    (* this expects to be called just prior to sending l3pkt*)
    let dst = (L3pkt.l3dst ~l3pkt) in
    let next_rt = (agent nexthop)#get_rtab in
    let this_sn = o2v (Rtab.seqno ~rt ~dst)
    and this_hc = o2v (Rtab.hopcount ~rt ~dst)
    and next_sn = o2v (Rtab.seqno ~rt:next_rt ~dst)
    and next_hc = o2v (Rtab.hopcount ~rt:next_rt ~dst)
    and ptype = 
      begin match (L3pkt.l3grepflags ~l3pkt) with
	| L3pkt.GREP_RREP  -> "rrep"
	| L3pkt.GREP_DATA -> "data" 
	| _ -> ""
      end
    in
    if not (
      (this_sn < next_sn) || 
      ((this_sn = next_sn) && (this_hc >= next_hc))
    ) then 
      let str = (Printf.sprintf "%s packet, this:%d, nexthoph:%d, dst:%d, this_sn: %d, this_hc: %d, next_sn: %d, next_hc:
    %d" ptype owner#id nexthop dst this_sn this_hc next_sn next_hc)
      in
      s#log_warning (lazy str);
      raise (Failure (Printf.sprintf "Inv_packet_upwards %s" str))
	
  )
    
  method private process_data_pkt 
    ~(l3pkt:L3pkt.l3packet_t) =  (
      
      let dst = (L3pkt.l3dst ~l3pkt) in
      begin try
	
	if (dst = owner#id) then ( (* pkt for us *)
	  s#hand_upper_layer ~l3pkt;
	  raise Break 
	);
	
	if  ((Rtab.repairing ~rt ~dst) || s#packets_waiting ~dst) then (
	  s#buffer_packet ~l3pkt;
	  raise Break
	);		

	begin try 
	  s#send_out ~l3pkt
	with 
	  | Send_Out_Failure -> 
	      begin
		s#log_notice 
		  (lazy (sprintf "Forwarding DATA pkt to dst %d failed, buffering."
		    dst));
		s#buffer_packet ~l3pkt;
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
    (* The use below of rreq_uid is intended to prevent the following race
       condition:
       
       Expanding ring search increases radius to say 16. A node which is 17
       hops away answers. We still have a pending send_rreq in the event loop
       (with ttl 32). Normally, when it fires, we would not do it because of
       the check (in send_rreq) for Rtab.repairing.
       But say that we have just started a new rreq phase for this
       destination, and we are currently at ttl 2. Then we would shoot ahead
       with a ttl 32. So we use these per-destination rreq_uids which are
       unique across a whole RREQ ERS. Maybe we could have used the dseqno
       instead, but uids seem safer.
    *)

    if (not (Rtab.repairing ~rt ~dst)) then (
    (* for the initial route request, we don't do it if there's already one
       going on for this destination (which isn't really expected to happen,
       but you never know) *)

      rreq_uids.(dst) <- Random.int max_int;


      Rtab.repair_start ~rt ~dst;
      s#log_notice 
      (lazy (sprintf "Initiating RREQ for dst %d" dst ));
      s#send_rreq ~ttl:_ERS_START_TTL ~dst ~rreq_uid:rreq_uids.(dst)
    )
  )

  method private send_rreq ~ttl ~dst ~rreq_uid = (
    
    if (rreq_uids.(dst) = rreq_uid && Rtab.repairing ~rt ~dst) then (
      s#log_notice (lazy (sprintf "Sending RREQ pkt for dst %d with ttl %d"
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
      let next_ttl = next_rreq_ttl ttl in
      let next_rreq_timeout = 
	((i2f (2 * next_ttl)) *. (hop_traversal_time())) in
      let next_rreq_event() = 
	  (s#send_rreq 
	    ~ttl:next_ttl
	    ~dst
	    ~rreq_uid
	  )
      in	
	s#send_out ~l3pkt;
	
	
(*	if next_rreq_ttl < ((Param.get Params.nodes)/10) then*)
	  (Gsched.sched())#sched_in ~f:next_rreq_event ~t:next_rreq_timeout;
    )
  )
    
  method private process_rrep_pkt 
    ~(l3pkt:L3pkt.l3packet_t)
    ~(sender:Common.nodeid_t)
    ~(fresh:bool)
    = (
      let update = s#newadv 
	~dst:(L3pkt.osrc ~l3pkt)
	~sn:(L3pkt.osn ~l3pkt)
	~hc:((L3pkt.ohc ~l3pkt) + (L3pkt.shc ~l3pkt))
	~nh:sender
      in 

      if (update || 
      (fresh && (
	(L3pkt.osrc ~l3pkt) = (L3pkt.l3src ~l3pkt))))
      then (
	(* the second line is for the case where the rrep was originated by the
	   source, in which case update=false (bc the info from it has already
	   been looked at in recv_l2pkt_hook) *)
	Rtab.repair_done ~rt ~dst:(L3pkt.osrc ~l3pkt);
	if ((L3pkt.l3dst ~l3pkt) <> owner#id) then (
	  try 
	    s#send_out ~l3pkt
	  with 
	    | Send_Out_Failure -> 
		s#log_notice 
		(lazy (sprintf "Forwarding RREP pkt to dst %d, obo %d failed, dropping"
		  (L3pkt.l3dst ~l3pkt)
		  (L3pkt.osrc ~l3pkt)));
	)
      )
    )
    
  method private send_out ~l3pkt = (
    
    let dst = L3pkt.l3dst ~l3pkt in
    assert (dst <> owner#id);
    assert (L3pkt.l3ttl ~l3pkt >= 0);
    assert (L3pkt.ssn ~l3pkt >= 1);

    let failed() = (
      L3pkt.decr_shc_pkt ~l3pkt;
      raise Send_Out_Failure
    ) in
    s#incr_seqno();
    L3pkt.incr_shc_pkt ~l3pkt;
    assert (L3pkt.shc ~l3pkt > 0);
    begin match (L3pkt.l3grepflags ~l3pkt) with
      | L3pkt.GREP_RADV 
      | L3pkt.GREP_RREQ -> 
	  assert (dst = L3pkt._L3_BCAST_ADDR);
	  L3pkt.decr_l3ttl ~l3pkt;
	  begin match ((L3pkt.l3ttl ~l3pkt) >= 0)  with
	    | true -> 
		Grep_hooks.sent_rreq() ;
		owner#mac_bcast_pkt ~l3pkt;
	    | false ->
		s#log_info (lazy (sprintf "Dropping packet (negative ttl)"));		
	  end
      | L3pkt.GREP_DATA
      | L3pkt.GREP_RREP ->
	  begin if ((L3pkt.l3grepflags ~l3pkt) = L3pkt.GREP_DATA) then (
	    Grep_hooks.sent_data();
	  ) else (
	    Grep_hooks.sent_rrep_rerr();
	  );
	    
	    let nexthop = 
	      match Rtab.nexthop ~rt ~dst  with
		| None -> failed()
		| Some nh -> nh 
	    in 
	    s#inv_packet_upwards ~nexthop:nexthop ~l3pkt;
	    try begin
	      owner#mac_send_pkt ~l3pkt ~dstid:nexthop; end
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
    Grep_hooks.recv_data();
     s#log_info (lazy (sprintf "Received app pkt from src %d"
	  (L3pkt.l3src ~l3pkt)));
  )

  (*
    method ctrl_hook action = (

    s#log_debug (sprintf "Originating dsdv (ttl 5) ");

    let pkt = 
      L3pkt.DSDV_PKT (L3pkt.make_dsdv_pkt 
	~srcid:owner#id 
	~originator:owner#id 
	~nhops:0
    ~seqno:seqno
    ~ttl:6) in
    
    seqno <- seqno + 1;
    owner#mac_bcast_pkt 
    ~l3pkt:pkt;
    )
  *)
    
    
  method private app_send l4pkt ~dst = (
    s#log_info (lazy (sprintf "Originating app pkt with dst %d"
      dst));
    let l3hdr = 
      L3pkt.make_l3hdr
	~srcid:owner#id
	~dstid:dst
	~ext:(L3pkt.make_grep_l3hdr_ext
	  ~flags:L3pkt.GREP_DATA
	  ~ssn:seqno
	  ~shc:0
	  ()
	)
	()
    in
    assert (dst <> owner#id);
    Grep_hooks.orig_data();
    let l3pkt = (L3pkt.make_l3pkt ~l3hdr:l3hdr ~l4pkt:l4pkt) in

    s#process_data_pkt ~l3pkt
  )




end
