(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)


(* XXX Currently broken: 

   Grep_agent has not been converted to use new mac-layer notification for send
   failures (see changelog), and therefore in the current state it alwyas
   thinks that unicast packets are succesfully xmitted ( because now
   node does not raise Mac_Send_Failure anymore on failed xmits).
*)



open Aodv_grep_common
open Printf
open Misc

let packet_buffer_size = Aodv_grep_common._PKTQUEUE_SIZE

type grep_state_t = 
  {
    seqno : int;
    hello_period : float option;
    rt : Rtab.t
  }


module Grep_stats = struct
  type stats = {
    mutable total_xmit : int; 
    mutable data_xmit : int; 
    mutable data_orig : int; 
    mutable data_recv : int; 
    mutable rreq_xmit : int; 
    mutable rrep_xmit : int; 
    mutable data_drop : int; 
  }
  let create_stats() = {
    total_xmit = 0; 
    data_xmit = 0; 
    data_orig = 0; 
    data_recv = 0; 
    rreq_xmit = 0; 
    rrep_xmit = 0; 
    data_drop = 0; 
  }

end


exception Send_Out_Failure


let agents_array_ = 
  Array.init Node.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let agents ?(stack=0) () = agents_array_.(stack)
let agent ?(stack=0) i = 
  Hashtbl.find agents_array_.(stack) i

module S = Grep_stats

class grep_agent ?(stack=0) theowner = 
object(s)

  inherit [S.stats] Rt_agent_base.base ~stack theowner 

  val mutable rt = Rtab.create_grep ~size:(Param.get Params.nodes) 
  val mutable seqno = 0
  val pktqs = Array.init (Param.get Params.nodes) (fun n -> Queue.create ())

  val mutable hello_period_ = None

  val rreq_uids = Array.create (Param.get Params.nodes) 0
    (* see #init_rreq for explanation on this *)

  val stats = S.create_stats()

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable) "/GREP_agent";
    Hashtbl.replace agents_array_.(stack) theowner#id (s :> grep_agent);
    s#incr_seqno()
  )

  method get_state () = 
    {seqno=seqno;
    hello_period = hello_period_;
    rt = rt}

  method set_state state = 
    seqno <- state.seqno;
    hello_period_ <- state.hello_period;
    rt <- state.rt
      
  method myid = myid
    
  method get_rtab = rt

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

  method private packets_waiting ~dst = 
    not (Queue.is_empty pktqs.(dst))

  method private queue_size() = 
    Array.fold_left (fun n q -> n + (Queue.length q))  0 pktqs

  method private send_waiting_packets ~dst = 
    while s#packets_waiting ~dst do
      let l3pkt = (Queue.pop pktqs.(dst)) in
      try 
	s#log_info 
	  (lazy (sprintf "Sending buffered `DATA pkt from src %d to dst %d."
	    (L3pkt.l3src l3pkt) dst));
	s#send_out ~l3pkt
      with 
	| Send_Out_Failure -> 
	    s#log_warning 
	    (lazy (sprintf "Sending buffered `DATA pkt from src %d to dst %d failed, dropping"
	      (L3pkt.l3src l3pkt) dst));
    done
    
  (* `DATA packets are buffered when they fail on send, 
     or if there are already buffered packets for that destination *)
  method private buffer_packet ~(l3pkt:L3pkt.t) = (
    match s#queue_size() < packet_buffer_size with 
      | true ->
	  let dst = L3pkt.l3dst l3pkt in
	  assert (dst <> L3pkt.l3_bcast_addr);
	  Queue.push l3pkt pktqs.(dst);
	  true;
      | false -> 
	  stats.S.data_drop <- stats.S.data_drop + 1;
	  s#log_notice (lazy (sprintf "Dropped packet for dst %d" 
	    (L3pkt.l3dst l3pkt)));
	  false
  )

  (* Wrapper around Rtab.newadv which additionally checks for 
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
      if (s#packets_waiting ~dst) then (
	s#send_waiting_packets ~dst
      );

    );
    update
  )

  method private packet_fresh ~l3pkt = (
    let grep_hdr = L3pkt.grep_hdr l3pkt in
    let pkt_ssn = Grep_pkt.ssn grep_hdr in
    match (Rtab.seqno ~rt ~dst:(L3pkt.l3src l3pkt)) with
      | None -> true 
      | Some s when (pkt_ssn > s) -> true
      | Some s when (pkt_ssn = s) -> 
	  Grep_pkt.shc grep_hdr
	  <
	  o2v (Rtab.hopcount ~rt ~dst:(L3pkt.l3src l3pkt))
      | Some s when (pkt_ssn < s) -> false
      | _ -> raise (Misc.Impossible_Case "Grep_agent.packet_fresh()")
  )
    
  method private recv_l3pkt_ ~l3pkt ~sender = (
    (* update route to source if packet came over fresher route than what we
       have *)
    let pkt_fresh = (s#packet_fresh ~l3pkt)
    and grep_hdr = L3pkt.grep_hdr l3pkt in
    let update =  
      s#newadv 
	~dst:(L3pkt.l3src l3pkt)
	~sn:(Grep_pkt.ssn grep_hdr)
	~hc:(Grep_pkt.shc grep_hdr)
	~nh:sender
    in
    assert (update = pkt_fresh);
    
    (* hand off to per-type method private *)
    begin match Grep_pkt.flags grep_hdr with
      | `DATA -> s#process_data_pkt ~l3pkt;
      | `RREQ -> s#process_rreq_pkt ~l3pkt ~fresh:pkt_fresh
      | `RADV -> s#process_radv_pkt ~l3pkt ~sender;
      | `RREP -> s#process_rrep_pkt ~l3pkt ~sender ~fresh:pkt_fresh;
    end
  )

  method private process_radv_pkt ~l3pkt ~sender = ()

  method recv_pkt_mac ~l2src ~l2dst l3pkt = (
    
    assert (L3pkt.l3ttl l3pkt >= 0);

    (* create or update 1-hop route to previous hop, unless the packet was
       originated by the previous hop, in which case this will happen in l3
       processing.
    *)

    if (l2src <> (L3pkt.l3src l3pkt)) then (
      let sender_seqno = 
	match Rtab.seqno ~rt ~dst:l2src with
	  | None -> 1
	  | Some n -> n + 1
      in
      let update =  
	s#newadv 
	  ~dst:l2src
	  ~sn:sender_seqno
	  ~hc:1
	  ~nh:l2src
      in
      assert (update);
    );
    
    s#recv_l3pkt_ ~l3pkt ~sender:l2src
  )


  method private process_rreq_pkt ~l3pkt ~fresh = (
    let grep_hdr = L3pkt.grep_hdr l3pkt in
    
    let rdst = (Grep_pkt.rdst grep_hdr) 
    and dsn =  (Grep_pkt.dsn grep_hdr) 
    in
    s#log_info 
      (lazy (sprintf "Received `RREQ pkt from src %d for dst %d"
	(L3pkt.l3src l3pkt) rdst));
    match fresh with 
      | true -> 
	  let answer_rreq = 
	    (rdst = myid)
	    ||
	    begin match (Rtab.seqno ~rt ~dst:rdst) with 
	      | None -> false
	      | Some s when (s > dsn) -> true
	      | Some s when (s = dsn) ->
		  (o2v (Rtab.hopcount ~rt ~dst:rdst)
		  <
		  (Grep_pkt.dhc grep_hdr) + Grep_pkt.shc grep_hdr)
	      | Some s when (s < dsn) -> false
	      | _ -> raise (Misc.Impossible_Case "Grep_agent.answer_rreq()") end
	  in
	  if (answer_rreq) then 
	    s#originate_rrep 
	      ~dst:(L3pkt.l3src l3pkt)
	      ~obo:rdst
	  else (* broadcast the rreq further along *)
	    s#send_out ~l3pkt
      | false -> 
	  s#log_info 
	  (lazy (sprintf "Dropping `RREQ pkt from src %d for dst %d (not fresh)"
	    (L3pkt.l3src l3pkt) rdst));
  )
    
  method private originate_rrep ~dst ~obo = (
    s#log_info 
    (lazy (sprintf "Sending `RREP pkt to dst %d, obo %d"
      dst obo));
    let grep_hdr = 
      Grep_pkt.make_grep_hdr
	~flags:`RREP
	~ssn:seqno
	~shc:0
	~osrc:obo
	~osn:(o2v (Rtab.seqno ~rt ~dst:obo))
	~ohc:(o2v (Rtab.hopcount ~rt ~dst:obo))
	()
    in
    let l3hdr = 
      L3pkt.make_l3hdr ~src:myid ~dst ~ext:(`GREP_HDR grep_hdr) () in

    let l3pkt =
      L3pkt.make_l3pkt ~l3hdr ~l4pkt:`EMPTY
    in
    try 
      s#send_out  ~l3pkt
    with 
      | Send_Out_Failure -> 
	  s#log_notice 
	  (lazy (sprintf "Sending `RREP pkt to dst %d, obo %d failed, dropping"
	    dst obo));
  )

  method private inv_packet_upwards ~nexthop ~l3pkt = (
    (* this expects to be called just prior to sending l3pkt*)
    let grep_hdr = L3pkt.grep_hdr l3pkt in

    let dst = (L3pkt.l3dst l3pkt) in
    let next_rt = (agent nexthop)#get_rtab in
    let this_sn = o2v (Rtab.seqno ~rt ~dst)
    and this_hc = o2v (Rtab.hopcount ~rt ~dst)
    and next_sn = o2v (Rtab.seqno ~rt:next_rt ~dst)
    and next_hc = o2v (Rtab.hopcount ~rt:next_rt ~dst)
    and ptype = 
      begin match (Grep_pkt.flags grep_hdr) with
	| `RREP  -> "rrep"
	| `DATA -> "data" 
	| `RREQ | `RADV -> ""
      end
    in
    if not (
      (this_sn < next_sn) || 
      ((this_sn = next_sn) && (this_hc >= next_hc))
    ) then 
      let str = (Printf.sprintf "%s packet, this:%d, nexthoph:%d, dst:%d, this_sn: %d, this_hc: %d, next_sn: %d, next_hc:
    %d" ptype myid nexthop dst this_sn this_hc next_sn next_hc)
      in
      s#log_warning (lazy str);
      raise (Failure (Printf.sprintf "Inv_packet_upwards %s" str))
	
  )
    
  method private process_data_pkt 
    ~(l3pkt:L3pkt.t) =  (
      
      let dst = (L3pkt.l3dst l3pkt) in
      begin try
	
	if (dst = myid) then ( (* pkt for us *)
	  s#hand_upper_layer ~l3pkt;
	  raise Break 
	);
	
	if  ((Rtab.repairing ~rt ~dst) || s#packets_waiting ~dst) then (
	  ignore (s#buffer_packet ~l3pkt); (* no difference whether
					      queue full or not *)
	  raise Break
	);		

	begin try 
	  s#send_out ~l3pkt
	with 
	  | Send_Out_Failure -> 
	      begin
		s#log_notice 
		  (lazy (sprintf "Forwarding `DATA pkt to dst %d failed, buffering."
		    dst));

		(* If packet was dropped, we don't initiate the route
		   repair.  *)
		if s#buffer_packet ~l3pkt then s#init_rreq  ~dst;
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
       unique across a whole `RREQ ERS. Maybe we could have used the dseqno
       instead, but uids seem safer.
    *)

    if (not (Rtab.repairing ~rt ~dst)) then (
      (* for the initial route request, we don't do it if there's already one
	 going on for this destination (which isn't really expected to happen,
	 but you never know) *)

      rreq_uids.(dst) <- Random.int max_int;

      Rtab.repair_start ~rt ~dst;
      s#log_notice 
	(lazy (sprintf "Initiating `RREQ for dst %d" dst ));
      s#send_rreq ~ttl:_ERS_START_TTL ~dst ~rreq_uid:rreq_uids.(dst)
    )
  )

  method start_hello () = 

    if Opt.is_none hello_period_ then begin
      hello_period_ <- Some _DEFAULT_HELLO_PERIOD;
      let jittered_start_time = 
	Random.float (o2v hello_period_)  in
      (Sched.s())#sched_in ~f:s#send_radv ~t:jittered_start_time;
    end

  method stop_hello () = 
    hello_period_ <- None

  method private send_radv() = (
    
    let grep_hdr = 
      Grep_pkt.make_grep_hdr
	~flags:`RADV
	~ssn:seqno
	~shc:0
	()
    in
    let l3hdr = 
      L3pkt.make_l3hdr ~src:myid ~dst:L3pkt.l3_bcast_addr 
	~ext:(`GREP_HDR grep_hdr) ~ttl:1 ()
    in
    let l3pkt = 
      L3pkt.make_l3pkt ~l3hdr ~l4pkt:`EMPTY

    in	
    s#send_out ~l3pkt;
    
    if (Opt.is_some hello_period_) then 
      let next_hello_t = 
	_DEFAULT_HELLO_PERIOD 
	+. Random.float (_HELLO_JITTER_INTERVAL())
	-. (_HELLO_JITTER_INTERVAL() /. 2.)
      in
      (Sched.s())#sched_in ~f:s#send_radv ~t:next_hello_t;
  )


  method private send_rreq ~ttl ~dst ~rreq_uid = (
    
    if (rreq_uids.(dst) = rreq_uid && Rtab.repairing ~rt ~dst) then (
      s#log_notice (lazy (sprintf "Sending `RREQ pkt for dst %d with ttl %d"
	dst ttl));
      
      let (dseqno, dhopcount) = 
	begin match (Rtab.seqno ~rt ~dst) with
	  | None -> (0, max_int)
	  | Some s -> (s, o2v (Rtab.hopcount ~rt ~dst)) end
      in
      let grep_hdr = 
	Grep_pkt.make_grep_hdr
	  ~flags:`RREQ
	  ~ssn:seqno
	  ~shc:0
	  ~rdst:dst
	  ~dsn:dseqno
	  ~dhc:dhopcount
	  ()
      in
      let l3hdr = 
	L3pkt.make_l3hdr ~src:myid ~dst:L3pkt.l3_bcast_addr
	  ~ext:(`GREP_HDR  grep_hdr) ~ttl:ttl ()
      in
      let l3pkt = 
	L3pkt.make_l3pkt ~l3hdr ~l4pkt:`EMPTY
      in
      let next_ttl = next_rreq_ttl ttl in
      let next_rreq_timeout = 
	((i2f (2 * ttl)) *. (hop_traversal_time s#bps)) in
      let next_rreq_event() = (s#send_rreq ~ttl:next_ttl ~dst ~rreq_uid) in

      s#send_out ~l3pkt;
      
      
      (*	if next_rreq_ttl < ((Param.get Params.nodes)/10) then*)
      (Sched.s())#sched_in ~f:next_rreq_event ~t:next_rreq_timeout;
    )
  )
    
  method private process_rrep_pkt 
    ~(l3pkt:L3pkt.t)
    ~(sender:Common.nodeid_t)
    ~(fresh:bool)
    = (
      let grep_hdr = L3pkt.grep_hdr l3pkt in

      let update = s#newadv 
	~dst:(Grep_pkt.osrc grep_hdr)
	~sn:(Grep_pkt.osn grep_hdr)
	~hc:((Grep_pkt.ohc grep_hdr) + (Grep_pkt.shc grep_hdr))
	~nh:sender
      in 

      if (update || 
      (fresh && (
	(Grep_pkt.osrc grep_hdr) = (L3pkt.l3src l3pkt))))
      then (
	(* the second line is for the case where the rrep was originated by the
	   source, in which case update=false (bc the info from it has already
	   been looked at in mac_recv_l2pkt) *)
	Rtab.repair_done ~rt ~dst:(Grep_pkt.osrc grep_hdr);
	if ((L3pkt.l3dst l3pkt) <> myid) then (
	  try 
	    s#send_out ~l3pkt
	  with 
	    | Send_Out_Failure -> 
		s#log_notice 
		(lazy (sprintf "Forwarding `RREP pkt to dst %d, obo %d failed, dropping"
		  (L3pkt.l3dst l3pkt)
		  (Grep_pkt.osrc grep_hdr)));
	)
      )
    )
    
  method private send_out ~l3pkt = (
    
    let dst = L3pkt.l3dst l3pkt in
    let grep_hdr = L3pkt.grep_hdr l3pkt in

    assert (dst <> myid);
    assert (L3pkt.l3ttl l3pkt >= 0);
    assert (Grep_pkt.ssn grep_hdr >= 1);

    let failed() = (
      Grep_pkt.decr_shc_pkt grep_hdr;
      raise Send_Out_Failure
    ) in
    s#incr_seqno();
    Grep_pkt.incr_shc_pkt grep_hdr;
    assert (Grep_pkt.shc grep_hdr > 0);
    stats.S.total_xmit <- stats.S.total_xmit + 1;
    begin match (Grep_pkt.flags grep_hdr) with
      | `RADV 
      | `RREQ -> 
	  assert (dst = L3pkt.l3_bcast_addr);
	  begin match ((L3pkt.l3ttl l3pkt) > 0)  with
	    | true -> 
		stats.S.rreq_xmit <- stats.S.rreq_xmit + 1;
		s#mac_bcast_pkt (L3pkt.decr_l3ttl l3pkt);
	    | false ->
		s#log_info (lazy (sprintf "Dropping packet (negative ttl)"));		
	  end
      | `DATA
      | `RREP ->
	  begin if ((Grep_pkt.flags grep_hdr) = `DATA) then (
	    stats.S.data_xmit <- stats.S.data_xmit + 1;
	  ) else (
	    stats.S.rrep_xmit <- stats.S.rrep_xmit + 1;
	  );
	    
	    let nexthop = 
	      match Rtab.nexthop ~rt ~dst  with
		| None -> failed()
		| Some nh -> nh 
	    in 
	    s#inv_packet_upwards ~nexthop:nexthop ~l3pkt;
	    (*try*)  s#mac_send_pkt l3pkt nexthop
	    (*with Node.Mac_Send_Failure -> failed()*)

	  end

    end
  )



  (* this is a null method because so far we don't need to model apps getting
     packets since we model CBR streams, and mhook catches packets as they enter
     the node *)
  method private hand_upper_layer ~l3pkt = (
    stats.S.data_recv <- stats.S.data_recv + 1;
    s#log_info (lazy (sprintf "Received app pkt from src %d"
      (L3pkt.l3src l3pkt)));
  )


  method recv_pkt_app l4pkt dst = (
    s#log_info (lazy (sprintf "Originating app pkt with dst %d"
      dst));
    let grep_hdr = 
      (Grep_pkt.make_grep_hdr
	~flags:`DATA
	~ssn:seqno
	~shc:0
	()
      )
    in
    let l3hdr = 
      L3pkt.make_l3hdr ~src:myid ~dst ~ext:(`GREP_HDR grep_hdr) () in

    assert (dst <> myid);
    stats.S.data_orig <- stats.S.data_orig + 1;
    let l3pkt = (L3pkt.make_l3pkt ~l3hdr:l3hdr ~l4pkt:l4pkt) in

    s#process_data_pkt ~l3pkt
  )

  method stats = stats

end
