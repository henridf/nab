(*
   in recv_l2pkt, we shouldn't update path to source if this is a data packet right??

   don't initiate rreq immediately on route error but wait till send next packet??
*)

(* meaning of flags:

   repairing: we sent a route request and have not since then received a reply
   for the node. 
   invalid: we noticed (through llacks) that the next hop is unreachable, or
   we received a RERR telling us the route to the dest is broken. an invalid
   entry can still have the destination seqno and the hopcount

   At the source, invalid and repairing are most of the time the same thing
   (though corner cases such as receive a data pkt from the dest, therefore we
   have a valid route but still have 'repairing' turned on).
   At an intermediate node, not necessarily the same thing, since the
   intermediate node is probably not doing local repair.

   packets_waiting: we have packets queued for that destination. 
   this should *not* be read as meaning the route is invalid, since it could
   just be congestion for example.
*)




(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Aodv_grep_common
open Printf
open Misc

let packet_buffer_size = 50

class type aodv_agent_t =
  object
    method private app_send : L4pkt.l4pkt_t -> dst:Common.nodeid_t -> unit
    method private buffer_packet : l3pkt:L3pkt.l3packet_t -> unit
    method private hand_upper_layer : l3pkt:L3pkt.l3packet_t -> unit
    method private incr_seqno : unit -> unit
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
    method private local_repair : 
      src:Common.nodeid_t -> 
      dst:Common.nodeid_t -> 
      bool
    method private process_rerr_pkt :
      l3pkt:L3pkt.l3packet_t -> 
      sender:Common.nodeid_t -> unit
    method private process_rreq_pkt :
      l3pkt:L3pkt.l3packet_t -> 
      fresh:bool -> unit
    method private recv_l2pkt_hook : L2pkt.l2packet_t -> unit
    method private send_out : l3pkt:L3pkt.l3packet_t -> unit
    method private send_rrep : dst:Common.nodeid_t -> obo:Common.nodeid_t -> unit
    method private send_rerr : dst:Common.nodeid_t -> obo:Common.nodeid_t -> unit
    method private send_rreq :
      ttl:int -> dst:Common.nodeid_t -> rreq_uid:int -> unit
    method private send_waiting_packets : dst:Common.nodeid_t -> unit
  end


exception Send_Out_Failure


let agents_array = ref ([||]:aodv_agent_t array)

let set_agents arr = agents_array := arr
let agent i = !agents_array.(i)



class aodv_agent owner : aodv_agent_t = 
object(s)

  inherit Log.inheritable_loggable

  val owner:#Simplenode.simplenode = owner
  val rt = Rtab.create_aodv ~size:(Param.get Params.nodes) 
  val mutable seqno = 0
  val pktqs = Array.init (Param.get Params.nodes) (fun n -> Queue.create()) 

  val rreq_uids = Array.create (Param.get Params.nodes) 0
    (* see #init_rreq for explanation on this *)


  initializer (
    s#set_objdescr ~owner  "/AODV_Agent";
    owner#add_recv_l2pkt_hook ~hook:s#recv_l2pkt_hook;
    owner#add_app_send_pkt_hook ~hook:s#app_send;
    s#incr_seqno()
  )

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

  method private local_repair ~src ~dst = false
(*
    let fwhops = o2v (Rtab.hopcount ~rt ~dst)
    and bwhops = o2v (Rtab.hopcount ~rt ~dst:src)
    in 
    if ((i2f fwhops) /. (i2f bwhops) < 0.5) then true else false
*)

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
	      s#log_error 
	      (lazy (sprintf "Sending buffered DATA pkt from src %d to dst %d failed, dropping"
		(L3pkt.l3src ~l3pkt:pkt) dst));
    done

  method private kill_buffered_packets ~dst = 
    while s#packets_waiting ~dst do
      ignore (Queue.pop pktqs.(dst));
      Grep_hooks.drop_data_rerr()
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
	)
      );
      update
    )



  (* as in paper *)
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
      | _ -> raise (Misc.Impossible_Case "Aodv_agent.packet_fresh()")
  )
    
   
  (* as recv_packet in paper *)
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
      | L3pkt.GREP_RERR -> s#process_rerr_pkt ~l3pkt ~sender;
      | L3pkt.NOT_GREP | L3pkt.EASE 
	  -> raise (Failure "Aodv_agent.recv_l2pkt_hook");
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
	      | Some s when (Rtab.invalid ~rt ~dst:rdst)
		  -> false
	      | Some s when  (s > dsn) (* Assume Destination-Only Flag always
					   set *)
		  -> true
	      | Some s when (s <= dsn) -> false
	      | _ -> raise (Misc.Impossible_Case "Aodv_agent.answer_rreq()") end
	  in
	  if (answer_rreq) then 
	    s#send_rrep 
	      ~dst:(L3pkt.l3src ~l3pkt)
	      ~obo:rdst
	  else (* broadcast the rreq further along *)
	    s#send_out ~l3pkt
      | false -> 
	  s#log_info (lazy (sprintf "Dropping RREQ pkt from src %d for dst %d (not fresh)"
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

  method private send_rerr ~dst ~obo = (
    s#log_info 
      (lazy (sprintf "Sending RERR pkt to dst %d, obo %d"
      dst obo));
    let grep_l3hdr_ext = 
      L3pkt.make_grep_l3hdr_ext 
	~flags:L3pkt.GREP_RERR
	~ssn:seqno
	~shc:0
	~rdst:obo
	~dsn:(o2v (Rtab.seqno ~rt ~dst:obo))
	~dhc:(o2v (Rtab.hopcount ~rt ~dst:obo))
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
	  (lazy (sprintf "Sending RERR pkt to dst %d, obo %d failed, dropping"
	    dst obo));
  )


  method private process_data_pkt 
    ~(l3pkt:L3pkt.l3packet_t) =  (
      let dst = (L3pkt.l3dst ~l3pkt) and 
	src = (L3pkt.l3src ~l3pkt) in
      begin try

	if (dst = owner#id) then ( (* pkt for us *)
	  s#hand_upper_layer ~l3pkt;
	  raise Break 
	);

	if (Rtab.repairing ~rt ~dst) || (s#packets_waiting ~dst) then (
	  s#buffer_packet ~l3pkt;
	  raise Break
	);		

	if (owner#id <> src) && (Rtab.invalid ~rt ~dst) then (
	  Grep_hooks.drop_data_rerr();
	  raise Break;
	);

	begin try 
	  s#send_out ~l3pkt
	with 
	  | Send_Out_Failure -> 
	      begin
		Rtab.invalidate ~rt ~dst;
		if (owner#id = src) || (s#local_repair ~dst ~src) then (
		  (* will need to check this when we do enable local_repairs *)
		  s#log_notice 
		  (lazy (sprintf "Forwarding DATA pkt to dst %d failed, buffering."
		    dst));
		  s#buffer_packet ~l3pkt;
		  let (dseqno,dhopcount) = 
		    begin match (Rtab.seqno ~rt ~dst) with
		      | None -> (0, max_int)
		      | Some s -> (s, o2v (Rtab.hopcount ~rt ~dst)) end
		  in
		  s#init_rreq 
		    ~dst 
		) else (
		  s#kill_buffered_packets ~dst;
		  Grep_hooks.drop_data_rerr();
		  s#send_rerr
		    ~dst:(L3pkt.l3src ~l3pkt)
		    ~obo:(L3pkt.l3dst ~l3pkt)
		)
	      end
	end
      with 
	| Break -> ()
	| e -> raise e;
      end;
      ()
    )

  method private init_rreq ~dst  = (
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
      s#log_info 
      (lazy (sprintf "Initializing RREQ for dst %d" dst ));
      s#send_rreq ~ttl:_ERS_START_TTL ~dst ~rreq_uid:rreq_uids.(dst)
    )
  )

  method private send_rreq ~ttl ~dst ~rreq_uid  = (
    
    if (rreq_uids.(dst) = rreq_uid && Rtab.repairing ~rt ~dst) then (
      s#log_info (lazy (sprintf "Sending RREQ pkt for dst %d with ttl %d"
	dst ttl));
      
      let dseqno, dhopcount = 
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
	((i2f next_ttl) *. (hop_traversal_time())) in
      let next_rreq_event() = 
	(s#send_rreq 
	  ~ttl:next_ttl
	  ~dst
	  ~rreq_uid
	)
      in	
      s#send_out ~l3pkt;
      (* we say that maximum 1-hop traversal is 20ms, 
	 ie half of value used by AODV. Another difference relative to AODV
	 is that we use ttl, not (ttl + 2).
	 This is ok while we use a simple MAC, and ok since our AODV impl 
	 will use the same values *)
      
      
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
	(L3pkt.osrc ~l3pkt) = (L3pkt.l3src ~l3pkt)))) then (
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

  method private process_rerr_pkt 
    ~(l3pkt:L3pkt.l3packet_t) 
    ~(sender:Common.nodeid_t) = (
      
      let invalid_dst = (L3pkt.rdst ~l3pkt) in
      Rtab.invalidate ~rt ~dst:invalid_dst;
      
      if ((L3pkt.l3dst ~l3pkt) <> owner#id) then (
	s#kill_buffered_packets ~dst:invalid_dst;
	try 
	  s#send_out ~l3pkt
	with 
	  | Send_Out_Failure -> ()
      ) else (

	let (dseqno,dhopcount) = 
	  begin 
	    match (Rtab.seqno ~rt ~dst:invalid_dst) with
	    | None -> (0, max_int)
	    | Some s -> (s, o2v (Rtab.hopcount ~rt ~dst:invalid_dst))
	  end

	in
	s#log_notice (lazy (sprintf "got a rerr for me, doing rreq for node %d hops %d seqno %d\n" invalid_dst dhopcount dseqno));
	s#init_rreq 
	  ~dst:invalid_dst
      )
    )


  method private send_out  ~l3pkt = (
    
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
	  begin
	    match ((L3pkt.l3ttl ~l3pkt) >= 0)  with
	      | true -> 
		  Grep_hooks.sent_rreq() ;
		  owner#mac_bcast_pkt ~l3pkt;
	      | false ->
		  s#log_info (lazy (sprintf "Dropping packet (negative ttl)"));		
	  end
      | L3pkt.GREP_DATA 
      | L3pkt.GREP_RERR 
      | L3pkt.GREP_RREP ->
	  begin if ((L3pkt.l3grepflags ~l3pkt) = L3pkt.GREP_DATA) then (
	    Grep_hooks.sent_data();
	  ) else (
	    Grep_hooks.sent_rrep_rerr();
	  );
	    if Rtab.invalid ~rt ~dst then failed();
	    let nexthop = 
	      match Rtab.nexthop ~rt ~dst  with
		| None -> raise (Failure "should have failed above")
		| Some nh -> nh
	    in 
	      try begin
		owner#mac_send_pkt ~l3pkt ~dstid:nexthop; end
	      with Simplenode.Mac_Send_Failure -> failed()
	  end

      | _ ->
	  raise (Failure "AODV_agent.send_out: unexpected packet type")
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

    s#process_data_pkt ~l3pkt;

  )
end
