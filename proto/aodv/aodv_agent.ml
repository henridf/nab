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




(* 
   str_agent 'enhancements' :


tests:

   make sure a node sends hellos when it should (part of active route, etc)
   do a random mix of mobility and traffic, and check that ttl drops on data
   packets don't happen (would indicate a loooop), and that asserts don't happen


 *)


open Aodv_pkt
open Aodv_defaults
open Printf
open Misc

module Aodv_stats = struct 
  type stats = {
    mutable total_xmit : int; 
    mutable data_xmit : int; 
    mutable data_orig : int; 
    mutable data_recv : int; 
    mutable hello_xmit : int;
    mutable rreq_xmit : int; 
    mutable rreq_init : int; 
    mutable rreq_orig : int; 
    mutable rerr_xmit : int; 
    mutable rerr_orig : int; 
    mutable rrep_xmit : int; 
    mutable rrep_orig : int; 
    mutable data_drop_overflow : int; 
    mutable data_drop_rerr : int; 
  }

let create_stats() = {
  total_xmit = 0; 
  data_xmit = 0; 
  data_orig = 0; 
  data_recv = 0;
  hello_xmit = 0;
  rreq_xmit = 0; 
  rreq_init = 0; 
  rreq_orig = 0; 
  rerr_xmit = 0; 
  rerr_orig = 0; 
  rrep_xmit = 0; 
  rrep_orig = 0; 
  data_drop_overflow = 0; 
  data_drop_rerr = 0; 
}

end


let agents_array_ = 
  Array.init Simplenode.max_nstacks 
    (fun _ -> Hashtbl.create (Param.get Params.nodes))

module S = Aodv_stats
  (* locally rename module Aodv_stats as 'S' to make things more compact each
     time we refer to a stats field. *)

class aodv_agent ?(stack=0) ?(localrepair=true) ?(dstonly=false) theowner  = 
object(s)


  inherit [S.stats] Rt_agent_base.base ~stack theowner 

  (* 
   *  Instance Variables. 
   *)

  val mutable myseqno = 0 (* our sequence number. *)

  val mutable last_bcast_time = Time.time() 
    (* We keep track of the last time since we sent any broadcastpacket in
       order to know when hello message is nec. (rfc 6.9)*)

  val default_rreq_flags = {g=true; d=dstonly; u=false}

  val rt = Aodv_rtab.create 100
    (* our routing table *)

  val pktqs : (Common.nodeid_t, L3pkt.t Queue.t) Hashtbl.t = 
    Hashtbl.create aodv_PKTQUEUE_SIZE

  val ers_uids = Hashtbl.create 100
    (* see send_rreq for explanation on this *)

  val mutable rreq_id = 0
  val rreq_cache : (Common.nodeid_t * int, Time.t) Hashtbl.t =
    Hashtbl.create 100 
      (* We keep a hashtable where key is the pair (originator, rreq_id), value
	 is the time this rreq was received, to discard duplicates in
	 process_rreq_pkt. *)

  val rreq_times : float Queue.t = Queue.create()
    (* We keep timestamps of the last RREQ_RATELIMIT route requests we have
       originated, in order to enforce that we emit no more than
       RREQ_RATELIMIT requests per second (rfc 6.3). *)

  val mutable stats = S.create_stats()

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/AODV_Agent";
    Hashtbl.replace agents_array_.(stack) theowner#id (s :> aodv_agent);
    s#incr_seqno();
    (Sched.s())#sched_in ~f:s#clean_data_structures
    ~t:(aodv_PATH_DISCOVERY_TIME +. 1.);
    (Sched.s())#sched_in ~f:s#send_hello
    ~t:(Random.float aodv_HELLO_INTERVAL);
  )

  (* 
   * Methods
   *)

  method myid = myid

  method private make_l3aodv ?(ttl=L3pkt.default_ttl) ?(l4pkt=`EMPTY) ~dst aodvhdr = 
  L3pkt.make_l3pkt 
    ~l3hdr:(L3pkt.make_l3hdr ~ttl ~src:myid ~dst ~ext:(`AODV_HDR aodvhdr) ())
    ~l4pkt

  method private send_hello() = (
    let now = Time.time() in
    let defer = now -. last_bcast_time < aodv_HELLO_INTERVAL in
    let last_bcast_time = match last_bcast_time with 0.0 -> now | t -> t in
    let next_t = 0.01 +. 
      if defer then (last_bcast_time +. aodv_HELLO_INTERVAL)
      else (now +. aodv_HELLO_INTERVAL) in
    
    if (not defer) && Aodv_rtab.have_active_route rt then (
      let rrep = make_rrep_hdr ~hc:0 ~dst:myid ~dst_sn:myseqno ~orig:myid
	~lifetime:(aodv_ALLOWED_HELLO_LOSS *. aodv_HELLO_INTERVAL) () in
      s#log_debug (lazy "Sending HELLO"); 

      s#send_out (s#make_l3aodv ~dst:L3pkt.l3_bcast_addr ~ttl:1 rrep);
    );

    (Sched.s())#sched_in ~f:s#send_hello ~t:next_t;
  )
      


  method private incr_seqno() = myseqno <- myseqno + 1

  (* Periodically go through rreq cache and remove rreqs which are too old to be
     of any use, to avoid that it fills up and gets to O(N) size. 
     Similarly, clean up ers_uid cache.
*)
  method private clean_data_structures() = (
    let oldest_acceptable = (Time.time()) -. aodv_PATH_DISCOVERY_TIME in
    let check key time = 
      if time < oldest_acceptable then (
	Hashtbl.remove rreq_cache key;
	assert (Hashtbl.find_all rreq_cache key = [])
      ) in
    Hashtbl.iter check rreq_cache;
    
    let check dst ers = 
      if not (Aodv_rtab.repairing rt dst) then (
	Hashtbl.remove ers_uids dst;
	assert (Hashtbl.find_all ers_uids dst = [])
      ) in
    Hashtbl.iter check ers_uids;

    (Sched.s())#sched_in ~f:s#clean_data_structures ~t:aodv_PATH_DISCOVERY_TIME;
  )

  method private packets_waiting ~dst = 
    try
      let q = Hashtbl.find pktqs dst in
      Queue.length q > 0
    with
	Not_found -> false

  method private queue_size() = 
    Hashtbl.fold (fun dst q  n -> n + (Queue.length q)) pktqs 0


  method private send_waiting_packets ~dst () = 
    if Aodv_rtab.valid rt dst then try 
      let pktqueue = Hashtbl.find pktqs dst in
      begin try 
	while true do
	  let l3pkt = Queue.pop pktqueue in
	  s#log_info 
	    (lazy (sprintf "Sending buffered DATA pkt from src %d to dst %d."
	      (L3pkt.l3src l3pkt) dst));
	  
	  Aodv_rtab.set_lifetime rt dst aodv_ACTIVE_ROUTE_TIMEOUT;
	  s#send_out l3pkt;
	done
      with Queue.Empty -> Hashtbl.remove pktqs dst 
      end;
    with Not_found -> ()
      
  method private after_send_any_buffered_pkts dst = 
    (Sched.s())#sched_at ~f:(s#send_waiting_packets ~dst) ~t:Scheduler.ASAP

  method private drop_buffered_packets ~dst = 
    try 
      let q = Hashtbl.find pktqs dst in
      stats.S.data_drop_rerr <- stats.S.data_drop_rerr + Queue.length q;
      Hashtbl.remove pktqs dst
    with Not_found -> ()

  (* DATA packets are buffered when they fail on send, 
     or if there are already buffered packets for that destination *)
  method private buffer_packet ~(l3pkt:L3pkt.t) = (
    let dst = L3pkt.l3dst l3pkt in
    assert (dst <> L3pkt.l3_bcast_addr);
    match s#queue_size() < aodv_PKTQUEUE_SIZE with 
      | true -> begin
	  try Queue.push l3pkt (Hashtbl.find pktqs dst)
	  with Not_found ->
	    let q = Queue.create() in
	    Queue.push l3pkt q;
	    Hashtbl.add pktqs dst q
	end
      | false -> 
	  stats.S.data_drop_overflow <- stats.S.data_drop_overflow + 1;
	  s#log_notice (lazy (sprintf "Dropped packet for dst %d" dst));
  )


  method private recv_l3pkt_ ~l3pkt ~sender = (
    
    let aodv_hdr = L3pkt.aodv_hdr l3pkt 
    and src = L3pkt.l3src l3pkt 
    and dst = L3pkt.l3dst l3pkt 
    and ttl = L3pkt.l3ttl l3pkt in 
    if aodv_hdr <> DATA then assert(sender = src);

    begin match aodv_hdr with
      | DATA -> s#process_data_pkt l3pkt sender;
      | RREQ rreq -> s#process_rreq_pkt src ttl rreq
      | RREP rrep -> s#process_rrep_pkt src ttl rrep ;
      | RERR rerr -> s#process_rerr_pkt src ttl rerr ;
      | RREP_ACK -> raise Misc.Not_Implemented;
    end
  )


  (* Entry point for incoming packets. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = 
    s#recv_l3pkt_ ~l3pkt ~sender:l2src



  (* return true if we can answer this rreq, as per rfc 6.6 *)
  method private can_reply rreq = 
    if rreq.rreq_dst = myid  then true else
      match Aodv_rtab.seqno rt rreq.rreq_dst, rreq.rreq_flags.d with 
	| Some sn, false -> 
	    Aodv_rtab.valid rt rreq.rreq_dst 
	    && (sn >=  rreq.rreq_dst_sn || rreq.rreq_flags.u)
	| _ -> false

	    
  (* originate route reply message *)
  method private send_rrep ~dst ~orig ~hc ~dst_sn lifetime = 
    match Aodv_rtab.nexthop_opt rt orig with
	Some nh -> 
	  let rrep = Aodv_pkt.make_rrep_hdr ~hc ~dst ~dst_sn ~orig ~lifetime () in
	  stats.S.rrep_orig <- stats.S.rrep_orig + 1;
	  s#send_out (s#make_l3aodv rrep ~dst:nh)
      | None -> 
	  s#log_error (lazy "aodv_agent.send_rrep"); 
	  failwith "aodv_agent.send_rrep"


  (* called when we have a route request which we can reply to. *)
  method private reply_rreq rreq = (
    s#log_info 
    (lazy (Printf.sprintf "Originating RREP for dst %d to originator %d"
      rreq.rreq_dst rreq.rreq_orig));
    
    (* There is a contradiction in the rfc regarding own seqno handling when
       we answer a RREQ:
       "If the generating node is the destination itself, it MUST increment
       its own sequence number by one if the sequence number in the RREQ
       packet is equal to that incremented value." (6.6.1)

       "Immediately before a destination node originates a RREP in
       response to a RREQ, it MUST update its own sequence number to the
       maximum of its current sequence number and the destination
       sequence number in the RREQ packet." (6.1)

       We do the latter.*)

    if rreq.rreq_dst = myid then
      myseqno <- max myseqno rreq.rreq_dst_sn;
    
    (* hopcount, dest sn, and lifetime fields in rrep are set differently
       depending if we are the destination of the rreq or not (6.6.1, 6.6.2) *)
    let hop_count, dst_sn, lifetime = 
      if rreq.rreq_dst = myid then 0, myseqno, aodv_MY_ROUTE_TIMEOUT
      else 
	Aodv_rtab.hopcount rt rreq.rreq_dst,
	(match Aodv_rtab.seqno rt rreq.rreq_dst with Some sn -> sn 
	  | None -> failwith "Aodv_agent.reply_rreq"), 
	Aodv_rtab.lifetime rt rreq.rreq_dst in
    
    (* should always have nexthop to rreq.rreq_orig, we just received rreq *)
    let nh = Aodv_rtab.nexthop rt rreq.rreq_orig in

    if rreq.rreq_dst <> myid then (
      (* update precursor lists as per 6.6.2 *)
      Aodv_rtab.add_precursor rt ~dst:rreq.rreq_dst ~pre:nh;
      Aodv_rtab.add_precursor rt ~dst:rreq.rreq_orig
	~pre:(Aodv_rtab.nexthop rt rreq.rreq_dst);
    );

    (* send rrep to request originator *)
    s#send_rrep ~hc:hop_count ~dst:rreq.rreq_dst ~dst_sn ~orig:rreq.rreq_orig
      lifetime;

    (* send out grat rrep if appropriate *)
    if rreq.rreq_dst <> myid && rreq.rreq_flags.g then (
      s#log_info 
      (lazy (Printf.sprintf "Originating GRAT RREP for dst %d to originator %d"
	rreq.rreq_dst rreq.rreq_orig));
      let grat_lifetime = Aodv_rtab.lifetime rt rreq.rreq_orig in
      let orig_hc = 
	match Aodv_rtab.hopcount_opt rt rreq.rreq_orig with
	  | Some hc -> hc
	  | None -> raise (Misc.Impossible_Case "Aodv_agent.reply_rreq") in 
      (* 'None' should be impossible because we have just received a rreq from
	 the destination. *) 
      s#send_rrep ~hc:orig_hc ~dst:rreq.rreq_orig ~dst_sn:rreq.rreq_orig_sn
	~orig:rreq.rreq_dst grat_lifetime
    )
  )


  method private process_rreq_pkt src ttl rreq = (
    (* Follows the steps from rfc 6.5 *)

    s#log_info (lazy (sprintf "Received RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst));

    (* 1. Update route to previous hop.*)
    Aodv_rtab.add_entry_neighbor rt src;
    s#after_send_any_buffered_pkts src;

    (* 2. If we have already received this RREQ, discard and don't do
       anything else. *)
    let discard = try 
      let last_time = Hashtbl.find rreq_cache (rreq.rreq_orig, rreq.rreq_id) in
      if last_time +. aodv_PATH_DISCOVERY_TIME > Time.time() then true 
      else false
    with Not_found -> false in

    Hashtbl.replace rreq_cache (rreq.rreq_orig, rreq.rreq_id) (Time.time());
    if discard then 
      s#log_debug (lazy (sprintf "Dropping RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst))
    else (
      (* 3. Otherwise, continue RREQ processing. 
	 First, create/update reverse path route to originator.*)
      Aodv_rtab.add_entry_rreq rt rreq src;
      (match Aodv_rtab.nexthop_opt rt rreq.rreq_orig with
	  None -> failwith "adding entry for rreq_orig did nothing"
	| Some a -> ());
      
      s#after_send_any_buffered_pkts rreq.rreq_orig;
	
      if s#can_reply rreq then s#reply_rreq rreq
      else if ttl > 0 then
	(* Update rreq fields as per rfc 6.5 and rebroadcast. *)
	let seqno = match Aodv_rtab.seqno rt rreq.rreq_dst with
	  | Some s -> max s rreq.rreq_dst_sn
	  | None -> rreq.rreq_dst_sn in
	let new_rreq = RREQ {rreq with 
	  rreq_hopcount = rreq.rreq_hopcount + 1; 
	  rreq_dst_sn = seqno} in
	let l3pkt = s#make_l3aodv 
	  ~dst:L3pkt.l3_bcast_addr ~ttl:(ttl - 1) new_rreq in

	s#send_out l3pkt
    )
  )

      
  (* Case (ii) of rfc 6.11 : received a pkt with dest for which we don't have
     valid route, and are not repairing either. 
     Actions:
     - Increment seqno for dst (NOP if we had no entry for dst)
     - Increase lifetime of route (NOP if we had no entry for dst)
     - Unicast rerr to source, will only have one invalid destination
  *)
  method private no_nexthop_rerr ~dst ~sender = (

    assert (not (Aodv_rtab.valid rt dst) && not (Aodv_rtab.repairing rt dst));
    Aodv_rtab.incr_seqno rt dst;

    let flags = {nd=false} 
    and seqno = Aodv_rtab.seqno rt dst in

    Aodv_rtab.set_lifetime rt dst aodv_DELETE_PERIOD;
    
    let aodv_hdr = Aodv_pkt.make_rerr_hdr ~flags [(dst, seqno)] in
    s#send_out (s#make_l3aodv aodv_hdr ~dst:sender)
  )


  method private process_data_pkt l3pkt sender = (
      let dst = (L3pkt.l3dst l3pkt) and src = (L3pkt.l3src l3pkt) in

	if (dst = myid) then s#hand_upper_layer ~l3pkt (* pkt for us *)

	else if (Aodv_rtab.repairing rt dst) || (s#packets_waiting ~dst) then 
	  s#buffer_packet ~l3pkt

	else match Aodv_rtab.valid rt dst, myid = src with
	  | false, true -> 
	      (* invalid, and we originated packet, so do rreq unless one
		 is already ongoing for this dst. *)
	      s#buffer_packet ~l3pkt; 
	      if (not (Aodv_rtab.repairing rt dst)) then s#init_rreq dst;
	  | false, false ->
	      (* some weirdo forwarded us a packet for an unkown dest. *)
	      s#no_nexthop_rerr ~dst ~sender
	  | true, _ -> 
	      (* route is valid, so we can fw data packet. *)
	      L3pkt.decr_l3ttl l3pkt;
	      if (L3pkt.l3ttl l3pkt < 0) then (
		s#log_error (lazy "DATA packet TTL went to 0. looop???");
		failwith "DATA packet TTL went to 0. looop???";
		(* exception can be removed later, only in beta testing to
		   make SURE i notice any such cases *)
	      ) else (
		Aodv_rtab.set_lifetime rt dst aodv_ACTIVE_ROUTE_TIMEOUT;
		s#send_out l3pkt;
	      )
    )

    
  method private init_rreq dst = (
    s#incr_seqno();
    stats.S.rreq_init <-  stats.S.rreq_init + 1;

    let ers_uid = Random.int max_int in (* explained in send_rreq *)
    Hashtbl.replace ers_uids dst ers_uid;

    Aodv_rtab.repair_start rt dst;
    s#log_info (lazy (sprintf "Originating RREQ for dst %d" dst));
    let start_ttl = match Aodv_rtab.hopcount_opt rt dst with 
      | Some hc -> aodv_TTL_INCREMENT + hc
      | None -> aodv_TTL_START in
    s#send_rreq ~local:false ~radius:start_ttl ~dst ~ers_uid ()
  )

  (* Originate local repair as per rfc 6.12 *)
  method private init_rreq_localrepair ~dst ~src = (
    s#incr_seqno();
    stats.S.rreq_init <-  stats.S.rreq_init + 1;

    let ers_uid = Random.int max_int in (* explained in send_rreq *)
    Hashtbl.replace ers_uids dst ers_uid;

    Aodv_rtab.repair_start rt dst;
    s#log_info (lazy (sprintf "Originating RREQ for dst %d (local repair)" dst));

    let aodv_MIN_REPAIR_TTL = Opt.default 0 (Aodv_rtab.hopcount_opt rt dst)
    and bw_hops = Opt.default 0 (Aodv_rtab.hopcount_opt rt src) in
    let start_ttl =  (max aodv_MIN_REPAIR_TTL bw_hops/2) + aodv_LOCAL_ADD_TTL in
    
    s#send_rreq ~local:true ~radius:start_ttl ~dst ~ers_uid ()
  )

  method private do_local_repair dst = 
    match Aodv_rtab.hopcount_opt rt dst with 
      | Some hc -> hc < aodv_MAX_REPAIR_TTL
      | None -> false

    
(* Originate a route error, when forwarding a data packet failed (either
   immediately if no local repair attempted, or after doing a local repair).
   nd is a boolean representing the 'No Delete' RERR flag.*)
  method private orig_rerr nd dst = (
    (* Follows the steps from rfc 6.11, case (i) *)


    let nexthop = Aodv_rtab.nexthop_invalid rt dst in
    (* since this method might end up getting called some time after the
       broken forwarding happened (if we attempted a local repair), the
       corresponding entry might have timed out and become invalid. *)

    (* All destinations whose nexthop is the broken hop are unreachable. *)
    let unreachables = Aodv_rtab.dests_thru_hop rt nexthop in 
    assert (List.mem dst unreachables);


    (* 2. Create new rerr packet. It should contain all the unreachable nodes
       satisfying 1. above *and* for which the precursor list is non-empty. 
       If there are no such nodes, we won't send RERR out.*)
    let in_new_rerr dst = Aodv_rtab.has_precursors rt dst in
    let unreachables_with_prec = (List.filter in_new_rerr unreachables) in
    let ur_wp_seqnos = List.map (Aodv_rtab.seqno rt) unreachables_with_prec in

    (* 3. For all the unreachable destinations (including those with no
       precursors), drop buffered packets, invalidate, update seqno, and
       update lifetime. This has to be done *after* step 2. because
       invalidating a route causes its precursor list to be expunged.

       Don't invalidate the actual destination to which forwarding failed if
       we have the 'N' (no delete) flag set. *)
    let update_dst thedst = 
      if not nd || thedst <> dst then (
	Aodv_rtab.invalidate rt thedst;
	s#drop_buffered_packets ~dst:thedst;
	Aodv_rtab.incr_seqno rt thedst;
	Aodv_rtab.set_lifetime rt thedst aodv_DELETE_PERIOD) in
    
      List.iter update_dst unreachables;

    if unreachables_with_prec <> [] then ( 
      (* at least 1 unreachable node has precursor *)

      let new_rerr = 
	Aodv_pkt.make_rerr_hdr ~flags:{nd=nd} 
	  (List.combine unreachables_with_prec ur_wp_seqnos) in

      (* If only 1 precursors for all unreachable nodes, RERR is
	 unicast, else RERR is broadcast. *)
      let dst = if Aodv_rtab.have_many_precursors rt unreachables_with_prec then
	L3pkt.l3_bcast_addr
      else List.hd (Aodv_rtab.precursors rt
 	(List.find (Aodv_rtab.has_precursors rt) unreachables_with_prec))  in
      let ttl = if dst = L3pkt.l3_bcast_addr then 1 else 255 in

      stats.S.rerr_orig <- stats.S.rerr_orig + 1;
      s#send_out (s#make_l3aodv ~dst ~ttl new_rerr)
    )

  )


  (* Callback from MAC layer when a unicast packet can not be delivered
     (for those MAC layers which support link-layer acknowledgements). *)
  method mac_callback l3pkt (nexthop : Common.nodeid_t) = 
    stats.S.total_xmit <- stats.S.total_xmit - 1;
    begin match L3pkt.aodv_hdr l3pkt with
      | DATA ->     
	  stats.S.data_xmit <- stats.S.data_xmit - 1;
	  let src, dst = L3pkt.l3src l3pkt, L3pkt.l3dst l3pkt in
	  List.iter (Aodv_rtab.invalidate rt) 
	    (Aodv_rtab.dests_thru_hop rt nexthop);

	  if src = myid then (
	    s#buffer_packet ~l3pkt; 
	    if (not (Aodv_rtab.repairing rt dst)) then 
	      s#init_rreq dst;
	  ) else if s#do_local_repair dst then (
	    s#buffer_packet ~l3pkt; 
	    if (not (Aodv_rtab.repairing rt dst)) then
	      s#init_rreq_localrepair ~dst ~src:(L3pkt.l3dst l3pkt)
	  ) else 
	    s#orig_rerr false dst 

      | RREP _ -> stats.S.rrep_xmit <- stats.S.rrep_xmit - 1;
      | RERR _ -> stats.S.rerr_xmit <- stats.S.rerr_xmit - 1;
      | RREQ _ -> raise (Misc.Impossible_Case "Aodv_agent.mac_callback"); 
	  (* rreqs are always broadcast *)
      | RREP_ACK -> ()
    end
    
  method private make_l3_rreq_pkt ~radius ~dst = (
    let flags = 
      {default_rreq_flags with u = ((Aodv_rtab.seqno rt dst) = None)} in
    
    let rreq = make_rreq_hdr ~flags  ~hc:0  ~rreq_id
      ~rreq_dst:dst
      ~rreq_dst_sn:(Opt.default 0 (Aodv_rtab.seqno rt dst))
      ~orig:myid
      ~orig_sn:myseqno () in

    s#make_l3aodv ~dst:L3pkt.l3_bcast_addr  ~ttl:(radius-1) rreq
  )


  (* Return true if we can send out a rreq without breaking RREQ_RATELIMIT
     (ie, can originate max RREQ_RATELIMIT rreqs per second). *)
  method private rreq_ratelimit_ok = 
    Queue.length rreq_times < aodv_RREQ_RATELIMIT ||
    Queue.peek rreq_times < Time.time() -. 1. 


  method private ers_uid dst uid = 
    try uid = Hashtbl.find ers_uids dst
    with Not_found -> false

  method private send_rreq ?(local=false) ?(beb=0) ~radius ~dst ~ers_uid () = (
    (* Note: ers_uid is a per-destination route request ID which is unique
       across a whole RREQ ERS (unlike rreq_id which is incremented each time
       we increase the radius and resend a route request).
       It is necessary to prevent the following race condition:
       
       Expanding ring search increases radius to say 16. A node which is 16
       hops away answers. We still have a pending send_rreq in the event loop
       (with ttl 32). Normally, when it fires, we would not do it because of
       the check (in send_rreq) for Aodv_rtab.repairing rt.
       But say that we have just started a new rreq phase for this
       destination, and we are currently at ttl 2. Then we would shoot ahead
       with a ttl 32. So we use these per-destination ers_uids which are
       unique across a whole RREQ ERS. 
    *)
    
    if beb lsr aodv_RREQ_RETRIES = 1 then (
      s#log_info (lazy (sprintf "Reached RREQ_RETRIES for dst %d, abandoning" dst));
      s#drop_buffered_packets ~dst;
      Aodv_rtab.repair_end rt dst
    ) else if s#ers_uid dst ers_uid && (Aodv_rtab.repairing rt dst) then
      if s#rreq_ratelimit_ok then (
	
	(* If we are done repairing this route (maybe a rreq from that node
	   arrived in the meantime) then we would not emit a new RREQ.*)
	s#log_info (lazy (sprintf 
	  "ERS RREQ pkt for dst %d with radius %d" dst radius));
	
	rreq_id <- rreq_id + 1;
	
	let l3pkt = s#make_l3_rreq_pkt ~radius ~dst in
	
	(* set up event for next rreq in ERS. *)
	let next_radius = 
	  if radius + aodv_TTL_INCREMENT <= aodv_TTL_THRESHOLD then
	    radius + aodv_TTL_INCREMENT else aodv_NET_DIAMETER in

	(* Figure out timeout for this rreq and (possibly) new Binary expo backoff
	   value *)
	let timeout, next_beb = 
	  match beb with 
	    | 0 -> aodv_RING_TRAVERSAL_TIME radius,
	      if radius = aodv_NET_DIAMETER then 2 else 0
	    | n -> ((float n) *. aodv_RING_TRAVERSAL_TIME radius, n * 2) in
		
	(* Setup event when this rreq times out. *)
	let evt = 
	  if local then 
	    (* If this was a local repair, then we will send a rerr back to
	       source, with 'no delete' flag set if repair was succesful *)
	    (fun _ -> 
	      Aodv_rtab.repair_end rt dst;
	      if Aodv_rtab.repairing rt dst then s#orig_rerr false dst
	      else s#orig_rerr true dst)
	  else 
	    (* Otherwise, we do another rreq *)
	    fun _ -> 
	      s#send_rreq ~beb:next_beb ~radius:next_radius ~dst ~ers_uid ()
	in

	stats.S.rreq_orig <- stats.S.rreq_orig + 1;
	(* Schedule the event *)
	(Sched.s())#sched_in ~f:evt ~t:timeout;

	(* Add to cache and sent out packet. *)
	Hashtbl.replace rreq_cache (myid, rreq_id) (Time.time());
	s#send_out l3pkt;
      ) else (
	(* we cannot send the RREQ immediately, because if so we would not be
	   respecting RREQ_RATELIMIT. So, we will try again in a little bit.*)
	s#log_info (lazy (sprintf 
	  "Delaying RREQ to %d by 0.2 due to rreq ratelimiting" dst));
	let evt() = s#send_rreq ~local ~beb ~radius ~dst ~ers_uid ()
	in (Sched.s())#sched_in ~f:evt ~t:0.2;
      )
  )
    

  method private process_rrep_pkt src ttl rrep  = (
    (* Follows the steps from rfc 6.7 *)

    s#log_info (lazy (sprintf "Received RREP pkt (originator %d, dst %d)"
	rrep.rrep_orig rrep.rrep_dst));

    (* 1. Update route to previous hop.*)
    Aodv_rtab.add_entry_neighbor rt src;
    s#after_send_any_buffered_pkts src;

    (* 2. Update route to dest for which this rrep advertises reachability.*)
    let updated = Aodv_rtab.add_entry_rrep rt rrep src in
    s#after_send_any_buffered_pkts rrep.rrep_dst;

    (* 3. We can forward rrep if we're not the originator, have updated the route
       above, and have a next hop. *)
    if updated && myid <> rrep.rrep_orig then (
      match Aodv_rtab.nexthop_opt rt rrep.rrep_orig with
	| None -> s#log_error 
	    (lazy "Cannot forward RREP because no next hop to originator")
	| Some nh -> 
	    if ttl > 0 then (
	      Aodv_rtab.add_precursor rt ~dst:rrep.rrep_dst ~pre:nh;
	      (* xxx According to 6.7 update below should be done but seems odd and 
		 inconsistent with what we do for other onehop routes we've created
		 Aodv_rtab.add_precursor rt ~dst:src ~pre:nh; *)
	      
	      let new_rrep = {rrep with rrep_hopcount = rrep.rrep_hopcount + 1} in
	      let l3pkt = s#make_l3aodv ~ttl:(ttl - 1) ~dst:nh (Aodv_pkt.RREP new_rrep) in
	      s#send_out l3pkt
	    )
    ) else if not updated then s#log_info
      (lazy "Cannot forward RREP because did not update route to destination.")
  )


  (* This is called either:
     1. When we receive a rerr from a neighbor, in which case the parameter [src]
     is the neighbor and rerr is the [rerr] header we received, or 
     2. When we notice a route break while forwarding, in which case the [rerr]
     header has been constructed locally, and [src] is the nexthop to whom
     forwarding failed.

     (it is possible to treat both cases jointly because the only difference
     is that we increment dst seqno in case 2.
  *)
  method private process_rerr_pkt src ttl rerr = (
    assert (List.length rerr.unreach >= 1);

    (* Follows the steps from rfc 6.11 *)

    (* 1. Compute list of unreachable destinations 
       (destinations in the RERR for which there exists a corresponding entry
       in the local routing table that has the transmitter of the received
       RERR as the next hop.) *)
    let check_unreach (dst, _) =  Aodv_rtab.nexthop_opt rt dst = Some src in
    let u = List.filter check_unreach rerr.unreach in


    (* 2. Create new rerr packet. It should contain all the unreachable nodes
       satisfying 1. above *and* for which the precursor list is non-empty. 
       If there are no such nodes, we won't send RERR out.*)
    let in_new_rerr (dst, _) =   Aodv_rtab.has_precursors rt dst in
      (* Aodv_rtab.nexthop rt dst = Some src already checked above *)
    let new_unreachables = (List.filter in_new_rerr u) in

    (* 3. For all these unreachable destinations, drop buffered packets,
       invalidate, update seqno, and update lifetime. This has to be done
       *after* step 2. because invalidating a route causes its precursor list
       to be expunged.
       
       This step is skipped if the 'N' flag (no delete) is set, indicating
       that a local repair is being done. *)
    if not rerr.rerr_flags.nd then
      let update_dst (dst, seqno) = 
	Aodv_rtab.invalidate rt dst;
	s#drop_buffered_packets ~dst;
	Opt.may	(Aodv_rtab.set_seqno rt dst) seqno;
	Aodv_rtab.set_lifetime rt dst aodv_DELETE_PERIOD in

      List.iter update_dst u;

    if new_unreachables <> [] then (* at least 1 unreachable node has precursor *)
      let new_rerr = 
	Aodv_pkt.make_rerr_hdr ~flags:rerr.rerr_flags new_unreachables in
      
      (* If only 1 precursors for all unreachable nodes, RERR is
	 unicast, else RERR is broadcast. *)
      let n_ur_nodes = fst (List.split new_unreachables) in

      let dst = if Aodv_rtab.have_many_precursors rt  n_ur_nodes  then
	L3pkt.l3_bcast_addr 
      else List.hd (Aodv_rtab.precursors rt
 	(List.find (Aodv_rtab.has_precursors rt) n_ur_nodes)) in
      let ttl = if dst = L3pkt.l3_bcast_addr then 1 else ttl - 1 in

      if ttl > 0 then 
	s#send_out (s#make_l3aodv ~dst ~ttl new_rerr)
  )


  method private send_out l3pkt = (
    
    let aodv_hdr = L3pkt.aodv_hdr l3pkt in
    let dst = L3pkt.l3dst l3pkt in
    assert (dst <> myid);
    assert (L3pkt.l3ttl l3pkt >= 0);

    (* a few sanity checks *)
    begin match aodv_hdr with
      | RREQ rreq -> assert (dst = L3pkt.l3_bcast_addr); 
	  assert(rreq.rreq_dst <> myid);
      | RERR rerr -> assert (List.length rerr.unreach > 0)
      | DATA | RREP_ACK | RREP _ -> ()
    end;

    stats.S.total_xmit <- stats.S.total_xmit + 1;

    (* for control packets, next hop is equal to src of IP (L3) header. 
       for data packets, next hop is read off of routing table. *)
    match aodv_hdr with 
      | DATA -> 
	  stats.S.data_xmit <- stats.S.data_xmit + 1;
	  s#mac_send_pkt l3pkt (Aodv_rtab.nexthop rt dst) 
      | RREP _ -> 
	  stats.S.rrep_xmit <- stats.S.rrep_xmit + 1;
	  s#mac_send_pkt l3pkt dst 
      | RREQ _ -> 
	  stats.S.rreq_xmit <- stats.S.rreq_xmit + 1;
	  assert (Queue.length rreq_times <= aodv_RREQ_RATELIMIT);
	  Queue.push (Time.time()) rreq_times;
	  if Queue.length rreq_times > aodv_RREQ_RATELIMIT then 
	    ignore (Queue.pop rreq_times);
	  last_bcast_time <- Time.time(); 
	  s#mac_bcast_pkt l3pkt
      | RERR _ -> 
	  stats.S.rerr_xmit <- stats.S.rerr_xmit + 1;
	  if dst = L3pkt.l3_bcast_addr then (
	    last_bcast_time <- Time.time(); 
	    s#mac_bcast_pkt l3pkt
	  ) else s#mac_send_pkt l3pkt dst
      | RREP_ACK -> raise Misc.Not_Implemented
  )


  (* this is a null method because so far we don't need to model apps getting
     packets since we model CBR streams, and mhook catches packets as they enter
     the node *)
  method private hand_upper_layer ~l3pkt = (
    stats.S.data_recv <- stats.S.data_recv + 1;
    s#log_info (lazy (sprintf "Received app pkt from src %d" (L3pkt.l3src l3pkt)));
  )

  
  method private recv_pkt_app (l4pkt : L4pkt.t) dst = (
    stats.S.data_orig <- stats.S.data_orig - 1;
    s#log_info (lazy (sprintf "Originating app pkt with dst %d" dst));
    let l3pkt = (s#make_l3aodv ~dst DATA) in
    if dst = myid then
      s#hand_upper_layer ~l3pkt
    else 
      s#process_data_pkt l3pkt myid;
  )

  method stats = stats

end
