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




(* notes:
   seqno can not be incremented according to incoming packets. (ie on rreq as
   for aodv)
   think about ttl initiation and incrementing for rreqs


   look into "cannot fw rreq bc did not update route to dest"
*)


   


open Str_pkt
open Str_defaults
open Printf
open Misc

let (>>>) = Str_rtab.(>>>)

module Str_stats = struct 
  type stats = {
    mutable total_xmit : int; 
    mutable data_xmit : int; 
    mutable data_orig : int; 
    mutable data_recv : int; 
    mutable hello_xmit : int;
    mutable rreq_xmit : int; 
    mutable rreq_init : int; 
    mutable rreq_orig : int; 
    mutable rrep_xmit : int; 
    mutable rrep_orig : int; 
    mutable data_drop_overflow : int; 
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
  rrep_xmit = 0; 
  rrep_orig = 0; 
  data_drop_overflow = 0; 
}

end


let agents_array_ = 
  Array.init Simplenode.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))

module S = Str_stats
  (* locally rename module Str_stats as 'S' to make things more compact each
     time we refer to a stats field. *)

class str_agent ?(stack=0) theowner  = 
object(s)


  inherit [S.stats] Rt_agent_base.base ~stack theowner 

  (* 
   *  Instance Variables. 
   *)

  val mutable last_bcast_time = Time.time() 
    (* We keep track of the last time since we sent any broadcastpacket in
       order to know when hello message is nec. (rfc 6.9)*)

  val rt = Str_rtab.create 100
    (* our routing table *)

  val pktqs : (Common.nodeid_t, L3pkt.t Queue.t) Hashtbl.t = 
    Hashtbl.create str_PKTQUEUE_SIZE

  val ers_uids = Hashtbl.create 100
    (* see send_rreq for explanation on this *)

  val rreq_times : float Queue.t = Queue.create()
    (* We keep timestamps of the last RREQ_RATELIMIT route requests we have
       originated, in order to enforce that we emit no more than
       RREQ_RATELIMIT requests per second (rfc 6.3). *)
    
  val mutable rreq_id = 0
  val rreq_cache : (Common.nodeid_t * int, Time.t) Hashtbl.t =
    Hashtbl.create 100 
      (* We keep a hashtable where key is the pair (originator, rreq_id), value
	 is the time this rreq was received, to discard duplicates in
	 process_rreq_pkt. *)
      
  val mutable stats = S.create_stats()
    
    
  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/STR_Agent";
    Hashtbl.replace agents_array_.(stack) theowner#id (s :> str_agent);
    (Sched.s())#sched_in ~f:s#clean_data_structures
    ~t:(str_PATH_DISCOVERY_TIME +. 1.);
    (Sched.s())#sched_in ~f:s#send_hello
    ~t:(Random.float str_HELLO_INTERVAL);
  )
    
  (* 
   *  Methods
   *)
    
  method myid = myid

  val mutable sn = 0
  val mutable last_sn_dtime = 0

  method private seqno() = 
    let dtime = Time.dtime() in 
    assert (dtime >= last_sn_dtime);
    if last_sn_dtime <> dtime then sn <- 0;
    last_sn_dtime <- dtime;
    {dtime=dtime; sn=sn}

  method private make_l3str ?(src=myid) ?(ttl=L3pkt.default_ttl) ?(l4pkt=`EMPTY) ~dst strhdr = 
    let ch = {prev_hop_sn=(s#seqno())} in
    L3pkt.make_l3pkt 
      ~l3hdr:(L3pkt.make_l3hdr ~ttl ~src ~dst 
	~ext:(`STR_HDR {ch=ch; ph=strhdr}) ()) ~l4pkt

  method private send_hello () = (
    let now = Time.time() in
    let defer = now -. last_bcast_time < str_HELLO_INTERVAL in
    let last_bcast_time = match last_bcast_time with 0.0 -> now | t -> t in
    let next_t = if defer then (last_bcast_time +. str_HELLO_INTERVAL +. 0.01)
    else (now +. str_HELLO_INTERVAL +. 0.01) in

    if not defer then (
      let l3pkt = s#make_l3str ~ttl:1 ~dst:L3pkt.l3_bcast_addr HELLO in
      s#log_debug (lazy "Sending HELLO");
      s#send_out l3pkt;
    );
    (Sched.s())#sched_at ~f:s#send_hello ~t:(Scheduler.Time next_t);
  )

  
  (* Periodically go through rreq cache and remove rreqs which are too old to be
     of any use, to avoid that it fills up and gets to O(N) size. 
     Similarly, clean up ers_uid cache.  *)
  method private clean_data_structures() = (
    let oldest_acceptable = (Time.time()) -. str_PATH_DISCOVERY_TIME in
    let check key time = 
      if time < oldest_acceptable then (
	Hashtbl.remove rreq_cache key;
	assert (Hashtbl.find_all rreq_cache key = [])
      ) in
    Hashtbl.iter check rreq_cache;

    let check dst ers = 
      if not (Str_rtab.repairing rt dst) then (
	Hashtbl.remove ers_uids dst;
	assert (Hashtbl.find_all ers_uids dst = [])
      ) in
    Hashtbl.iter check ers_uids;

    (Sched.s())#sched_in ~f:s#clean_data_structures ~t:str_PATH_DISCOVERY_TIME;
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
    if Str_rtab.valid rt dst then try 
      let pktqueue = Hashtbl.find pktqs dst in
      begin try 
	while true do
	  let l3pkt = Queue.pop pktqueue in
	  s#log_info 
	    (lazy (sprintf "Sending buffered DATA pkt from src %d to dst %d."
	      (L3pkt.l3src l3pkt) dst));
	  
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
      Hashtbl.remove pktqs dst
    with Not_found -> ()

  (* DATA packets are buffered when they fail on send, 
     or if there are already buffered packets for that destination *)
  method private buffer_packet ~(l3pkt:L3pkt.t) = (
    let dst = L3pkt.l3dst l3pkt in
    assert (dst <> L3pkt.l3_bcast_addr);
    match s#queue_size() < str_PKTQUEUE_SIZE with 
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

    (* update route to previous hop. *)
    let sender_sn = (L3pkt.str_hdr l3pkt).ch.prev_hop_sn in
    let fresh =  Str_rtab.add_entry rt ~dst:sender ~seqno:sender_sn ~nh:sender
      ~hc:1 in
    if fresh then s#after_send_any_buffered_pkts sender;

    let str_hdr = (L3pkt.str_hdr l3pkt).ph
    and src = L3pkt.l3src l3pkt 
    and dst = L3pkt.l3dst l3pkt in 
    begin match str_hdr with
      | RREQ _ | RREP _ | HELLO -> assert(sender = src)
      | DATA _ -> ()
    end;

    begin match str_hdr with
      | HELLO -> ()
      | DATA data_hdr -> s#process_data_pkt data_hdr l3pkt sender;
      | RREQ rreq -> s#process_rreq_pkt src (L3pkt.l3ttl l3pkt) rreq
      | RREP rrep -> s#process_rrep_pkt src (L3pkt.l3ttl l3pkt) rrep sender;
    end
  )

  (* Entry point for incoming packets. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = 
    s#recv_l3pkt_ ~l3pkt ~sender:l2src



  (* return true if we can answer this rreq, as per rfc 6.6 *)
  method private can_reply rreq = 
    if rreq.rreq_dst = myid  then true else
      match Str_rtab.seqno rt rreq.rreq_dst, false with 
	  (* dstonly flag would go in place of 'false' above *)
	| Some sn, false -> 
	    Str_rtab.valid rt rreq.rreq_dst 
	    && sn >=  rreq.rreq_dst_sn
	| _ -> false

	    
  (* originate route reply message *)
  method private send_rrep ~dst ~orig ~hc ~dst_sn = 
    match Str_rtab.nexthop_maybe rt orig with
	Some nh -> 
	  let rrep = Str_pkt.make_rrep_hdr ~hc ~dst ~dst_sn ~orig  in
	  let l3pkt = s#make_l3str rrep ~dst:nh in
	  stats.S.rrep_orig <- stats.S.rrep_orig + 1;
	  s#send_out l3pkt
      | None -> 
	  s#log_error (lazy "str_agent.send_rrep"); 
	  failwith "str_agent.send_rrep"


  (* called when we have a route request which we can reply to. *)
  method private reply_rreq rreq = (
    s#log_info 
    (lazy (Printf.sprintf "Originating RREP for dst %d to originator %d"
      rreq.rreq_dst rreq.rreq_orig));
    
    let hop_count, dst_sn = 
      if rreq.rreq_dst = myid then 0, (s#seqno())
      else 
	Str_rtab.hopcount rt rreq.rreq_dst,
	(match Str_rtab.seqno rt rreq.rreq_dst with Some sn -> sn 
	  | None -> failwith "Str_agent.reply_rreq") in
    
    (* should always have nexthop to rreq.rreq_orig, we just received rreq *)
    let nh = Str_rtab.nexthop rt rreq.rreq_orig in

    (* send rrep to request originator *)
    s#send_rrep ~hc:hop_count ~dst:rreq.rreq_dst ~dst_sn ~orig:rreq.rreq_orig;
  )

  method private process_rreq_pkt src ttl rreq = (

    s#log_info (lazy (sprintf "Received RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst));

    (* First, create/update reverse path route to originator.*)
    let fresh = Str_rtab.add_entry rt 
      ~dst:rreq.rreq_orig
      ~seqno:rreq.rreq_orig_sn
      ~hc:(rreq.rreq_hopcount + 1)
      ~nh:src
    in
    if fresh then s#after_send_any_buffered_pkts rreq.rreq_orig;

    (* If we have already received this RREQ, we will discard it without doing
       anything else. *)
    let discard = try 
      let last_time = Hashtbl.find rreq_cache (rreq.rreq_orig, rreq.rreq_id) in
      if last_time +. str_PATH_DISCOVERY_TIME > Time.time() then true 
      else false
    with Not_found -> false in

    Hashtbl.replace rreq_cache (rreq.rreq_orig, rreq.rreq_id) (Time.time());

    if discard then  
      s#log_debug (lazy (sprintf "Dropping RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst))
    else (
    (* 3. Otherwise, continue RREQ processing. *)
(*
    begin try 
      ignore (Str_rtab.nexthop rt rreq.rreq_orig)

    with Not_found -> Printf.printf 
      "Not_found. Me: %d, Fresh: %b, Repairing: %b, have_entry: %b Orig: %d, rreq_seqno: %s, current_seqno: %s rreq_dist: %d, current_dist: %d\n"
      myid fresh 
      (Str_rtab.repairing rt rreq.rreq_orig)
      (Str_rtab.have_entry rt rreq.rreq_orig)
      rreq.rreq_orig (sprint_seqno rreq.rreq_orig_sn)
      (sprint_seqno (Opt.get (Str_rtab.seqno rt rreq.rreq_orig)))
      (rreq.rreq_hopcount + 1)
      (Str_rtab.hopcount rt rreq.rreq_orig)
    end;
*)
      if s#can_reply rreq then (
      s#reply_rreq rreq )
      else if ttl > 0 then
	let new_rreq = 
	  RREQ {rreq with rreq_hopcount=rreq.rreq_hopcount + 1} in
	let l3pkt = 
	  s#make_l3str ~ttl:(ttl - 1) ~dst:L3pkt.l3_bcast_addr new_rreq in
	s#send_out l3pkt
    )
  )

      
  method private process_data_pkt data_hdr l3pkt sender = (

    let dst = (L3pkt.l3dst l3pkt) and src = (L3pkt.l3src l3pkt) in
    
    (* Add/update entry to node which originated this packet. *)
    if Str_rtab.add_entry rt ~dst:src ~nh:sender 
      ~seqno:data_hdr.data_src_sn ~hc:(data_hdr.data_hopcount + 1)
    then
      s#after_send_any_buffered_pkts src;

    let new_data_hdr = 
      {data_hdr with data_hopcount = data_hdr.data_hopcount + 1} in
    let new_l3pkt = s#make_l3str ~l4pkt:(L3pkt.l4pkt l3pkt) 
      ~dst ~src ~ttl:(L3pkt.l3ttl l3pkt) (DATA new_data_hdr) in

    if (dst = myid) then s#hand_upper_layer ~l3pkt:new_l3pkt (* pkt for us *)
      
    else if (Str_rtab.repairing rt dst) || (s#packets_waiting ~dst) then 
      s#buffer_packet ~l3pkt:new_l3pkt
	
    else match Str_rtab.valid rt dst with
      | false -> 
	  (* invalid, and we originated packet, so do rreq unless one
	     is already ongoing for this dst. *)
	  s#buffer_packet ~l3pkt:new_l3pkt; 
	  if (not (Str_rtab.repairing rt dst)) then s#init_rreq dst;
      | true -> 
	  (* route is valid, so we can fw data packet. *)
	  L3pkt.decr_l3ttl new_l3pkt;
	  if (L3pkt.l3ttl l3pkt < 0) then (
	    s#log_error (lazy "DATA packet TTL went to 0. looop???");
	    failwith "DATA packet TTL went to 0. looop???";
	    (* exception can be removed later, only in beta testing to
	       make SURE i notice any such cases *)
	  ) else s#send_out new_l3pkt;
  )

    
  method private init_rreq dst = (
    stats.S.rreq_init <-  stats.S.rreq_init + 1;

    let ers_uid = Random.int max_int in (* explained in send_rreq *)
    Hashtbl.replace ers_uids dst ers_uid;

    Str_rtab.repair_start rt dst;
    s#log_info (lazy (sprintf "Originating RREQ for dst %d" dst));
    let start_ttl = match Str_rtab.hopcount_maybe rt dst with 
      | Some hc -> str_TTL_INCREMENT + hc
      | None -> str_TTL_START in
    s#send_rreq  ~radius:start_ttl ~dst ~ers_uid ()
  )

    
  (* Callback from MAC layer when a unicast packet can not be delivered
     (for those MAC layers which support link-layer acknowledgements). *)
  method mac_callback l3pkt (nexthop : Common.nodeid_t) = 
    stats.S.total_xmit <- stats.S.total_xmit - 1;
    begin match (L3pkt.str_hdr l3pkt).ph with
      | DATA _ ->     
	  stats.S.data_xmit <- stats.S.data_xmit - 1;
	  let src, dst = L3pkt.l3src l3pkt, L3pkt.l3dst l3pkt in
	  List.iter (Str_rtab.invalidate rt) 
	    (Str_rtab.dests_thru_hop rt nexthop);
	  s#buffer_packet ~l3pkt; 
	  if (not (Str_rtab.repairing rt dst)) then s#init_rreq dst
		
      | RREP _ -> stats.S.rrep_xmit <- stats.S.rrep_xmit - 1;
      | HELLO | RREQ _ -> raise (Misc.Impossible_Case "Str_agent.mac_callback"); 
	  (* rreqs and hellos are always broadcast *)
    end
    
  method private make_l3_rreq_pkt ~radius ~dst = (
    let rreq = make_rreq_hdr ~hc:0 ~rreq_id ~rreq_dst:dst
      ~rreq_dst_sn:(Opt.default null_seqno (Str_rtab.seqno rt dst))
      ~rreq_dst_hc:(Opt.default max_int (Str_rtab.nexthop_maybe rt dst))
      ~orig:myid
      ~orig_sn:(s#seqno())  in

    s#make_l3str ~ttl:radius ~dst:L3pkt.l3_bcast_addr rreq
  )


  (* Return true if we can send out a rreq without breaking RREQ_RATELIMIT
     (ie, can originate max RREQ_RATELIMIT rreqs per second). *)
  method private rreq_ratelimit_ok = 
    Queue.length rreq_times < str_RREQ_RATELIMIT ||
    Queue.peek rreq_times < Time.time() -. 1. 


  method private ers_uid dst uid = 
    try uid = Hashtbl.find ers_uids dst
    with Not_found -> false

  method private send_rreq  ?(beb=0) ~radius ~dst ~ers_uid () = (
    (* Note: ers_uid is a per-destination route request ID which is unique
       across a whole RREQ ERS.
       It is necessary to prevent the following race condition:
       
       Expanding ring search increases radius to say 16. A node which is 16
       hops away answers. We still have a pending send_rreq in the event loop
       (with ttl 32). Normally, when it fires, we would not do it because of
       the check (in send_rreq) for Str_rtab.repairing rt.
       But say that we have just started a new rreq phase for this
       destination, and we are currently at ttl 2. Then we would shoot ahead
       with a ttl 32. So we use these per-destination ers_uids which are
       unique across a whole RREQ ERS. 
    *)
    
    if beb lsr str_RREQ_RETRIES = 1 then (
      s#log_info (lazy (sprintf "Reached RREQ_RETRIES for dst %d, abandoning" dst));
      s#drop_buffered_packets ~dst;
      Str_rtab.repair_end rt dst
    ) else if s#ers_uid dst ers_uid && (Str_rtab.repairing rt dst) then
      if s#rreq_ratelimit_ok then (
	
	(* If we are done repairing this route (maybe a rreq from that node
	   arrived in the meantime) then we would not emit a new RREQ.*)
	s#log_info (lazy (sprintf 
	  "ERS RREQ pkt for dst %d with radius %d" dst radius));

	rreq_id <- rreq_id + 1;

	let l3pkt = s#make_l3_rreq_pkt ~radius ~dst in
	
	(* set up event for next rreq in ERS. *)
	let next_radius = 
	  if radius + str_TTL_INCREMENT <= str_TTL_THRESHOLD then
	    radius + str_TTL_INCREMENT else str_NET_DIAMETER in

	(* Figure out timeout for this rreq and (possibly) new Binary expo backoff
	   value *)
	let timeout, next_beb = 
	  match beb with 
	    | 0 -> str_RING_TRAVERSAL_TIME radius,
	      if radius = str_NET_DIAMETER then 2 else 0
	    | n -> ((float n) *. str_RING_TRAVERSAL_TIME radius, n * 2) in
		
	(* Setup event when this rreq times out. *)
	let evt = 
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
	let evt() = s#send_rreq ~beb ~radius ~dst ~ers_uid ()
	in (Sched.s())#sched_in ~f:evt ~t:0.2;
      )
  )
    

  method private process_rrep_pkt src ttl rrep sender = (

    s#log_info (lazy (sprintf "Received RREP pkt (originator %d, dst %d)"
	rrep.rrep_orig rrep.rrep_dst));

    (* Add/update entry to dest for which this rrep advertises reachability.*)
    let updated =  Str_rtab.add_entry rt ~dst:rrep.rrep_dst ~nh:sender 
      ~seqno:rrep.rrep_dst_sn ~hc:(rrep.rrep_hopcount+1) in
    if updated then
      s#after_send_any_buffered_pkts rrep.rrep_dst;

    (* We can forward rrep if we're not the originator, have updated the route
       above, and have a next hop. *)
    if updated && myid <> rrep.rrep_orig then (
      match Str_rtab.nexthop_maybe rt rrep.rrep_orig with
	| None -> s#log_error 
	    (lazy "Cannot forward RREP because no next hop to originator")
	| Some nh ->
	    if ttl > 0 then (
	      let new_rrep = 
		Str_pkt.RREP {rrep with rrep_hopcount=rrep.rrep_hopcount+1} in
	      let l3pkt =  s#make_l3str ~ttl:(ttl-1) ~dst:nh new_rrep in
	      s#send_out l3pkt
	    )
    ) else if not updated then s#log_info (lazy 
	"Cannot forward RREP because did not update route to destination.")
  )


    method private send_out l3pkt = (
      
      sn <- sn + 1;
      let str_hdr = L3pkt.str_hdr l3pkt in
      let dst = L3pkt.l3dst l3pkt in
      assert (dst <> myid);
      assert (L3pkt.l3ttl l3pkt >= 0);
      
      (* a few sanity checks *)
      begin match str_hdr.ph with
	| RREQ rreq -> assert (dst = L3pkt.l3_bcast_addr); 
	    assert(rreq.rreq_dst <> myid);
	| HELLO | DATA _ | RREP _ -> ()
      end;

      stats.S.total_xmit <- stats.S.total_xmit + 1;

      (* for control packets, next hop is equal to src of IP (L3) header. 
	 for data packets, next hop is read off of routing table. *)
      match str_hdr.ph with 
	| DATA _ -> 
	    stats.S.data_xmit <- stats.S.data_xmit + 1;
	    s#mac_send_pkt l3pkt (Str_rtab.nexthop rt dst) 
	| RREP _ -> 
	    stats.S.rrep_xmit <- stats.S.rrep_xmit + 1;
	    s#mac_send_pkt l3pkt dst 
	| RREQ _ -> 
	    stats.S.rreq_xmit <- stats.S.rreq_xmit + 1;
	    assert (Queue.length rreq_times <= str_RREQ_RATELIMIT);
	    Queue.push (Time.time()) rreq_times;
	    if Queue.length rreq_times > str_RREQ_RATELIMIT then 
	      ignore (Queue.pop rreq_times);
	    last_bcast_time <- Time.time(); 
	    s#mac_bcast_pkt l3pkt
	| HELLO -> 
	    stats.S.hello_xmit <- stats.S.hello_xmit + 1;
	    last_bcast_time <- Time.time(); 
	    s#mac_bcast_pkt l3pkt
    )


  (* this is a null method because so far we don't need to model apps getting
     packets since we model CBR streams, and mhook catches packets as they enter
     the node *)
  method private hand_upper_layer ~l3pkt = (
    stats.S.data_recv <- stats.S.data_recv + 1;
    s#log_info (lazy (sprintf "Received app pkt from src %d" (L3pkt.l3src l3pkt)));
  )

  
  method private recv_pkt_app (l4pkt : L4pkt.t) dst = (
    assert (dst <> myid);
    stats.S.data_orig <- stats.S.data_orig - 1;

    s#log_info (lazy (sprintf "Originating app pkt with dst %d" dst));
    let str_hdr = {data_src_sn=(s#seqno()); data_hopcount=0} in
    let l3pkt = s#make_l3str ~l4pkt ~dst (DATA str_hdr) in
    s#process_data_pkt str_hdr l3pkt myid;
  )

  method stats = stats

end

let () = Time.maintain_discrete_time()
