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
   notes:
   seqno can not be incremented according to incoming packets. (ie on rreq as
   for aodv)
   think about ttl initiation and incrementing for rreqs
   
   look into "cannot fw rreq bc did not update route to dest"


   right now, we only call str_rtab.repair_end on RREP-caused calls to
   add_entry. need to think through carefully if we should do this 
   when we call add_entry in other cases.
   
   not sure that setting max_int, max_float in recv_pkt_app won't give
   overflow problems when computing binding_metric...


   in can_reply, should maybe take into account distance back to rreq originator
   (ie estimate time it would take for rreq to travel back) to avoid sending
   back a rrep with only slightly better cost, which ends up not having better
   cost by the time it arrives at the originator (because the age of the
   originator's entry has increased in the meantime).


*)


(* 
   todo before testing: 

   fill in binding_metric_ in str_rtab


   - notion of time: 

   to check loop-free invariant (that a packet always goes to a node with
   lower ST-cost to the destination), we could put destination age/distance in
   header for all unicast packets, and have receiver always check invariant,
   drop packet (and log error) otherwise.



   [need to think through what to do with the valid/invalid distinction.] for
   now, the only way for an entry to be invalidated is when the fwding to the
   nexthop fails (and in this case all entries with this nexthop are
   invalidated). don't really think that adding timeouts (as in AODV) will
   make a noticeable difference (we would save the step the failed fwding, but
   for the rest no difference i thinks).

*)


open Str_pkt
open Str_defaults
open Printf
open Misc

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
    
  val mutable send_hellos = true

  method stop_hello = send_hellos <- false
  method start_hello = send_hellos <- true


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

  method private make_l3str ?(src=myid) ?(ttl=L3pkt.default_ttl) ?(l4pkt=`EMPTY) ~dst strhdr = 
    L3pkt.make_l3pkt 
      ~l3hdr:(L3pkt.make_l3hdr ~ttl ~src ~dst 
	~ext:(`STR_HDR strhdr) ()) ~l4pkt

  method private send_hello () = (
    let now = Time.time() in
    let defer = now -. last_bcast_time < str_HELLO_INTERVAL in
    let last_bcast_time = match last_bcast_time with 0.0 -> now | t -> t in
    let next_t = if defer then (last_bcast_time +. str_HELLO_INTERVAL +. 0.01)
    else (now +. str_HELLO_INTERVAL +. 0.01) in

    if not defer && send_hellos then (
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

  method private packets_waiting dst = 
    try
      let q = Hashtbl.find pktqs dst in
      Queue.length q > 0
    with
	Not_found -> false

  method private queue_size() = 
    Hashtbl.fold (fun dst q  n -> n + (Queue.length q)) pktqs 0


  method private send_waiting_packets ~dst () = 
    let pktq = try Hashtbl.find pktqs dst with Not_found -> Queue.create() in
    let newq = Queue.create() in
    begin try 
      while true do
	let l3pkt = Queue.pop pktq in
	let src = L3pkt.l3src l3pkt 
	and dst = L3pkt.l3dst l3pkt 
	and str_hdr = L3pkt.str_hdr l3pkt in
	let data_hdr =  match str_hdr with
	  | DATA data_hdr -> data_hdr;
	  | HELLO | RREQ _ | RREP _ -> 
	      failwith "Str_agent#send_waiting_packets" in
	
	(* some code here is duplicated in process_data_packet *)
	let cost = Str_rtab.cost (data_hdr.data_dst_age, data_hdr.data_dst_hc)
	in
	match Str_rtab.have_better_valid_route rt cost dst with
	  | true -> 	 
	      let age, hc = Str_rtab.valid_age_dist rt dst in
	      let new_data_hdr = 
		{data_hdr with (* data hopcount has already been increased in
				  process_data_pkt *)
		  data_dst_age = age;
		  data_dst_hc = hc} in
	      let new_l3pkt = s#make_l3str ~l4pkt:(L3pkt.l4pkt l3pkt) 
		~dst ~src ~ttl:(L3pkt.l3ttl l3pkt - 1) (DATA new_data_hdr) in
	      s#log_info (lazy 
		(Printf.sprintf "Forwarding buffered data packet"));
	      s#send_out new_l3pkt;
	  | false -> 
	      s#log_notice (lazy "Invariant violated on buffered packet!"); 
	      s#log_notice (lazy (Printf.sprintf "my best valid entry %s"
		(Str_rtab.sprint_best_valid_entry rt dst)));
	      s#log_notice (lazy (Printf.sprintf 
		"cost on data packet: %f (age: %f, hc: %d)" 
		cost
		data_hdr.data_dst_age
		data_hdr.data_dst_hc));
	      Queue.push l3pkt newq
      done
    with Queue.Empty -> 
      Hashtbl.remove pktqs dst;
      if not (Queue.is_empty newq) then Hashtbl.add pktqs dst newq
	
    end
      
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
    let _ = Str_rtab.add_entry rt ~dst:sender ~age:0.0 ~nh:sender ~hc:1 in
    
    s#after_send_any_buffered_pkts sender;

    let str_hdr = L3pkt.str_hdr l3pkt
    and src = L3pkt.l3src l3pkt 
    and dst = L3pkt.l3dst l3pkt in 
    begin match str_hdr with
      | RREQ _ | RREP _ | HELLO -> assert(sender = src)
      | DATA _ -> ()
    end;

    begin match str_hdr with
      | HELLO -> ()
      | DATA data_hdr -> 
	  let data_hdr = 
	    {data_hdr with data_hopcount = data_hdr.data_hopcount + 1} in
	  let l3pkt = s#make_l3str ~l4pkt:(L3pkt.l4pkt l3pkt) 
	    ~dst ~src ~ttl:(L3pkt.l3ttl l3pkt) (DATA data_hdr) in
	  s#process_data_pkt ~local:false data_hdr l3pkt sender;
      | RREQ rreq -> s#process_rreq_pkt src (L3pkt.l3ttl l3pkt) rreq
      | RREP rrep -> s#process_rrep_pkt src (L3pkt.l3ttl l3pkt) rrep sender;
    end
  )

  (* Entry point for incoming packets. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = 
    s#recv_l3pkt_ ~l3pkt ~sender:l2src



  (* return true if we can answer this rreq, ie if we have a route with lower
     binding cost. 

     assumes that the hopcount field on the rreq has *not* yet been incremented.
  *)
  method private can_reply rreq = 
    let offset = rreq.rreq_hopcount + 1 in
    let rreq_cost = 
      if (rreq.rreq_dst_age = unknown_age) || (rreq.rreq_dst_hc = max_int)
      then max_float else Str_rtab.cost 
	(rreq.rreq_dst_age, rreq.rreq_dst_hc) in
    let have_better_route = 
      Str_rtab.have_better_route rt ~offset rreq_cost rreq.rreq_dst in
    if have_better_route then (
      let age, hopcount = 
	if rreq.rreq_dst = myid then 0.0, 0
	else Str_rtab.age_dist rt rreq.rreq_dst in
      s#log_info 
	(lazy (Printf.sprintf 
	  "Can answer RREQ for dst %d to originator %d" rreq.rreq_dst
	  rreq.rreq_orig));
      s#log_info 
	(lazy (Printf.sprintf 
	  "RREQ has age=%.2f, dist = dst_hc=%d, cost=%f"
	  rreq.rreq_dst_age rreq.rreq_dst_hc rreq_cost));
      s#log_info 
	(lazy (Printf.sprintf 
	  "We have route RREQ with age=%.2f, (dist=%d  + rreq_hc=%d + 1), cost=%f"
	  age hopcount offset (Str_rtab.cost (age, hopcount + offset))));
    );
    rreq.rreq_dst = myid || have_better_route


	    
  (* originate route reply message *)
  method private send_rrep ~dst ~orig ~dst_hc ~dst_age = 
    match Str_rtab.nexthop_opt rt orig with
	Some nh -> 
	  Str_rtab.using_entry rt orig;
	  let rrep = 
	    Str_pkt.make_rrep_hdr ~replier:myid ~hc:0 ~dst ~dst_hc ~dst_age ~orig in


	  let l3pkt = s#make_l3str rrep ~dst:nh in
	  stats.S.rrep_orig <- stats.S.rrep_orig + 1;
	  s#send_out l3pkt
      | None -> 
	  s#log_error (lazy "str_agent.send_rrep"); 
	  failwith "str_agent.send_rrep"


  (* Called when we have a route request which we can reply to. 
     Assumes that we have a valid entry for the originator.
  *)
  method private reply_rreq rreq = (
    let age, hopcount = 
      if rreq.rreq_dst = myid then 0.0, 0
      else Str_rtab.age_dist rt rreq.rreq_dst  in
    
    s#log_info 
      (lazy (Printf.sprintf 
	"Originating RREP for dst %d to originator %d" rreq.rreq_dst rreq.rreq_orig));
    
    (* send rrep to request originator *)
    s#send_rrep ~dst_hc:hopcount ~dst:rreq.rreq_dst ~dst_age:age ~orig:rreq.rreq_orig;
  )

  method private process_rreq_pkt src ttl rreq = (

    s#log_info (lazy (sprintf "Received RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst));

    (* First, create/update reverse path route to originator.*)
    let fresh = Str_rtab.add_entry rt 
      ~dst:rreq.rreq_orig
      ~hc:(rreq.rreq_hopcount + 1)
      ~age:rreq.rreq_orig_age
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
      if s#can_reply rreq then 
	s#reply_rreq rreq 
      else if ttl > 0 then
	let new_rreq = 
	  RREQ {rreq with rreq_hopcount=rreq.rreq_hopcount + 1} in
	let l3pkt = 
	  s#make_l3str ~ttl:(ttl - 1) ~dst:L3pkt.l3_bcast_addr new_rreq in
	s#send_out l3pkt
    )
  )

    
(* this method assumes that the data_hopcount field of the STR data hdr has
   already been incremented. *)
  method private process_data_pkt ~local data_hdr l3pkt sender = (
    
    let dst = (L3pkt.l3dst l3pkt) and src = (L3pkt.l3src l3pkt) in
    
    (* Add/update entry to node which originated this packet. *)
    if Str_rtab.add_entry rt ~dst:src ~nh:sender 
      ~age:data_hdr.data_src_age ~hc:(data_hdr.data_hopcount + 1) 
    then
      s#after_send_any_buffered_pkts src;

    if (dst = myid) then 
      (* pkt is for us *)
      s#hand_upper_layer ~l3pkt:l3pkt 

    else if (Str_rtab.repairing rt dst) || (s#packets_waiting dst) then 
      s#buffer_packet ~l3pkt

    else 
      (* basic data forwarding as follows:
	 - if we have a valid entry with lower cost, forward it onto that route.
	 - if we have an invalid entry with lower cost, start rreq.
	 - if we have no entry (valid or not) for which cost decreases compared to
	 previous hop's cost, then we have an invariant.
      *)
      let cost = Str_rtab.cost (data_hdr.data_dst_age, data_hdr.data_dst_hc)
      in

      (* some code here is duplicated in send_waiting_packets *)
      match Str_rtab.have_better_valid_route rt cost dst with
	| true -> 	 
	    let age, hc = Str_rtab.valid_age_dist rt dst in
	    let new_data_hdr = 
	      {data_hdr with 
		data_dst_age = age;
		data_dst_hc = hc} in
	    let new_l3pkt = s#make_l3str ~l4pkt:(L3pkt.l4pkt l3pkt) 
	      ~dst ~src ~ttl:(L3pkt.l3ttl l3pkt - 1) (DATA new_data_hdr) in
	    s#send_out new_l3pkt;
	| false -> 
	    match Str_rtab.have_better_route rt cost dst || local with
	      | true -> (* invariant is not violated, but our route is
			   invalid, so buffer and do rreq if none ongoing for
			   this dest.*)
		  s#buffer_packet ~l3pkt; 
		  if (not (Str_rtab.repairing rt dst)) then s#init_rreq dst;
	      | false -> 
		  s#log_error (lazy "Invariant violated!");
		  s#log_error (lazy (Printf.sprintf "my best entry %s"
		    (Str_rtab.sprint_best_entry rt dst)));
		  s#log_error (lazy (Printf.sprintf 
		    "cost on data packet: %f (age: %f, hc: %d)" 
		    cost
		    data_hdr.data_dst_age
		    data_hdr.data_dst_hc));
  )

    
  method private init_rreq dst = (
    stats.S.rreq_init <-  stats.S.rreq_init + 1;

    let ers_uid = Random.int max_int in (* explained in send_rreq *)
    Hashtbl.replace ers_uids dst ers_uid;

    Str_rtab.repair_start rt dst;
    s#log_info (lazy (sprintf "Originating RREQ for dst %d" dst));
    let start_ttl = match Str_rtab.hopcount_opt rt dst with 
      | Some hc -> str_TTL_INCREMENT + hc
      | None -> str_TTL_START in
    s#send_rreq  ~radius:start_ttl ~dst ~ers_uid ()
  )

    
  (* Callback from MAC layer when a unicast packet can not be delivered
     (for those MAC layers which support link-layer acknowledgements). *)
  method mac_callback l3pkt (nexthop : Common.nodeid_t) = 
    stats.S.total_xmit <- stats.S.total_xmit - 1;
    begin match L3pkt.str_hdr l3pkt with
      | DATA _ ->     
	  stats.S.data_xmit <- stats.S.data_xmit - 1;
	  let src, dst = L3pkt.l3src l3pkt, L3pkt.l3dst l3pkt in
	  Str_rtab.invalidate_nexthop rt nexthop;
	  s#buffer_packet ~l3pkt; 
	  if (not (Str_rtab.repairing rt dst)) then s#init_rreq dst

      | RREP _ -> stats.S.rrep_xmit <- stats.S.rrep_xmit - 1;
      | HELLO | RREQ _ -> raise (Misc.Impossible_Case "Str_agent.mac_callback"); 
	  (* rreqs and hellos are always broadcast *)
    end
    
  method private make_l3_rreq_pkt ~radius ~dst = (
    let age, dist = 
      try (Str_rtab.age_dist rt dst)
      with Not_found -> (unknown_age, max_int) in
    let rreq = make_rreq_hdr ~hc:0 ~rreq_id ~rreq_dst:dst
      ~rreq_dst_age:age
      ~rreq_dst_hc:dist
      ~orig:myid
      ~orig_age:0.0  in

    s#make_l3str ~ttl:(radius-1) ~dst:L3pkt.l3_bcast_addr rreq
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

    s#log_info (lazy (sprintf 
      "Received RREP pkt (replier %d originator %d, dst %d)"
      rrep.rrep_replier rrep.rrep_orig rrep.rrep_dst));

    (* Add/update entry to dest for which this rrep advertises reachability.*)
    let updated = 
      Str_rtab.add_entry rt ~dst:rrep.rrep_dst ~nh:sender 
	~age:rrep.rrep_dst_age ~hc:(rrep.rrep_dst_hc + rrep.rrep_hopcount)
    in
    if updated && rrep.rrep_orig = myid then (
      s#after_send_any_buffered_pkts rrep.rrep_dst;
      (*      if (not (s#packets_waiting rrep.rrep_dst)) then*)
	Str_rtab.repair_end rt rrep.rrep_dst;
    );
    (* We can forward rrep if we're not the originator, have updated the route
       above, and have a next hop. *)
    if updated && myid <> rrep.rrep_orig then (
      match Str_rtab.nexthop_opt rt rrep.rrep_orig with
	| None -> s#log_error 
	    (lazy "Cannot forward RREP because no next hop to originator")
	| Some nh ->
	    if ttl > 0 then (
	      Str_rtab.using_entry rt rrep.rrep_orig;
	      let new_rrep = 
		Str_pkt.RREP {rrep with rrep_hopcount=rrep.rrep_hopcount+1} in
	      let l3pkt =  s#make_l3str ~ttl:(ttl-1) ~dst:nh new_rrep in
	      s#send_out l3pkt
	    )
    ) else if not updated && myid <> rrep.rrep_orig then s#log_info (lazy 
      "Cannot forward RREP because did not update route to destination.")
  )


  method private send_out l3pkt = (
    
    let str_hdr = L3pkt.str_hdr l3pkt in
    let dst = L3pkt.l3dst l3pkt in
    assert (dst <> myid);
    assert (L3pkt.l3ttl l3pkt >= 0);
    
    (* a few sanity checks *)
    begin match str_hdr with
      | RREQ rreq -> assert (dst = L3pkt.l3_bcast_addr);
	  assert(rreq.rreq_dst <> myid);
      | HELLO | DATA _ | RREP _ -> ()
    end;

    stats.S.total_xmit <- stats.S.total_xmit + 1;

    (* for control packets, next hop is equal to src of IP (L3) header. 
       for data packets, next hop is read off of routing table. *)
    match str_hdr with 
      | DATA _ -> 
	  stats.S.data_xmit <- stats.S.data_xmit + 1;
	  let nh = (Str_rtab.nexthop rt dst) in
	  s#log_info (lazy 
	    (sprintf "Forwarding DATA pkt from src %d to dst %d, nexthop %d."
	      (L3pkt.l3src l3pkt) dst nh));
	  Str_rtab.using_entry rt dst;
	  s#mac_send_pkt l3pkt nh
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
    stats.S.data_orig <- stats.S.data_orig - 1;
    s#log_info (lazy (sprintf "Originating app pkt with dst %d" dst));
    let str_hdr = {
      data_src_age=0.0; 
      data_hopcount=0;
      data_dst_age=max_float;
      data_dst_hc=max_int
    } in
    let l3pkt = s#make_l3str ~l4pkt ~dst (DATA str_hdr) in
    if dst = myid then
      s#hand_upper_layer ~l3pkt
    else 
      s#process_data_pkt ~local:true str_hdr l3pkt myid;
  )

  method stats = stats

end

let () = Time.maintain_discrete_time()
