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




   xxx todo:

   currently disabled updating hop to previous node for non-HELLo packets.
   In order to re-enable, either 
   - explicify seqno of previous hop in str header. 
   - would it work to simply put add previous hop as valid, with seqno (-1)?
   for forwarding, it would work, but what if we try to answer a RREQ with
   this? seems fragile.
   - if have any entry with 'real' seqno, simply take seqno as previous
   seqno + 1, and add valid entry with this seqno.


   timeout for an entry to become invalid should be > than 2 * max RREQ timeout,
   in order to minimize # times a node has no entry to fw RREP to originator
   
   usable invalid routes: this is based on some timeout since last_used of an
   invalid route.
   But, we need some way to invalidate an invalid route when next hop fails?
   Maybe not, because when it fails, we would then need a better (valid or
   invalid route) than what is marked on the packet. And *our* invalid route
   in our rtab cannot be better than the invalid route in the packet right?

   for bidir traffic, updating route on incoming data packets will lose,
   because as soon as src sends a 2nd packet, nodes on the route
   have a higher seqno than others (which heard the RREQ), and so if there is
   a breakage, all valid routes from the previous RREQ are useless.
   but, simply removing the update-entry-to-originator rule isn't good either,
   because then nodes don't learn the reverse route. Would a grat rrep be the
   solution?

*)


open Str_pkt
open Str_defaults
open Printf
open Misc

let sp = Printf.sprintf

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
    mutable rrep_drop_nohop : int; 
    mutable data_drop : int; 
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
    rrep_drop_nohop = 0;
    data_drop = 0; 
  }

  let add s1 s2 = {
    total_xmit = s1.total_xmit + s2.total_xmit; 
    data_xmit = s1.data_xmit + s2.data_xmit; 
    data_orig = s1.data_orig + s2.data_orig; 
    data_recv = s1.data_recv + s2.data_recv;
    hello_xmit = s1.hello_xmit + s2.hello_xmit;
    rreq_xmit = s1.rreq_xmit + s2.rreq_xmit; 
    rreq_init = s1.rreq_init + s2.rreq_init; 
    rreq_orig = s1.rreq_orig + s2.rreq_orig; 
    rrep_xmit = s1.rrep_xmit + s2.rrep_xmit; 
    rrep_orig = s1.rrep_orig + s2.rrep_orig; 
    rrep_drop_nohop = s1.rrep_drop_nohop + s2.rrep_drop_nohop;
    data_drop = s1.data_drop + s2.data_drop; 
  }
end




module S = Str_stats
  (* locally rename module Str_stats as 'S' to make things more compact each
     time we refer to a stats field. *)


type persist_t = 
    {rt : Str_rtab.t;
    seqno : int;
    metric : Str_rtab.metric_t;
    send_hellos : bool;
    stats : S.stats
    }

let agents_array_ = 
  Array.init Node.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
    

class str_agent ?(stack=0) ?(state : persist_t option) (metric : Str_rtab.metric_t) theowner = 
object(s)

  inherit [S.stats, persist_t] Rt_agent_base.base_persist ~stack theowner 

  (* 
   *  Instance Variables. 
   *)

  val mutable last_bcast_time = Time.time()
    (* We keep track of the last time since we sent any broadcastpacket in
       order to know when hello message is nec. (rfc 6.9)*)


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
      
  (* These instance vars are saved and restored. *)
  val rt = 
    if state = None then Str_rtab.create 100 else (Opt.get state).rt
      (* our routing table *)
  val mutable stats = 
    if state = None then S.create_stats() else (Opt.get state).stats

  val mutable send_hellos = 
    if state = None then true else (Opt.get state).send_hellos

  val mutable seqno = if state = None then 0 else (Opt.get state).seqno

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/str";
    Hashtbl.replace agents_array_.(stack) theowner#id (s :> str_agent);
    (Sched.s())#sched_in ~f:s#clean_data_structures
    ~t:(str_PATH_DISCOVERY_TIME +. 1.);
    (Sched.s())#sched_in ~f:s#send_hello
    ~t:(Random.float str_HELLO_INTERVAL);
  )

  method stop_hello = send_hellos <- false
  method start_hello = send_hellos <- true
  method clear_stats = stats <- S.create_stats()

  method private incr_seqno = seqno <- seqno + 1

  (* 
   *  Methods
   *)
    
  method myid = myid

  method rtab_metric = rt, metric
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
      let l3pkt = s#make_l3str ~ttl:1 ~dst:L3pkt.l3_bcast_addr (HELLO seqno) in
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
      if not (s#repairing dst) then (
	Hashtbl.remove ers_uids dst;
	assert (Hashtbl.find_all ers_uids dst = [])
      ) in
    Hashtbl.iter check ers_uids;
    
    (Sched.s())#sched_in ~f:s#clean_data_structures ~t:str_PATH_DISCOVERY_TIME;
  )

  method repairing dst = 
    try
      let q = Hashtbl.find pktqs dst in
      Queue.length q > 0
    with
	Not_found -> false

  method private queue_size() = 
    Hashtbl.fold (fun dst q  n -> n + (Queue.length q)) pktqs 0


(* iterate over all packets:
   let i_ent = best invalid route from rtab.
   let usable_i_ent = best usable invalid route from rtab.
   let v_ent = best valid route from rtab.

   - if v_ent is better than on packet, will forward through v_ent
   else if usable_i_ent is better than on packet, will forward through v_ent 
   else will leave packet in queue

   if forwarding via v_ent, then update str.i_ent on packet with our i_ent if
   ours is better. process_data_pkt should also do this trick.

*)
      
      
  method private send_waiting_packets ~dst () = 
    let pktq = try Hashtbl.find pktqs dst with Not_found -> Queue.create() in
    let f l3pkt = 
      let str_hdr = L3pkt.str_hdr l3pkt in
      let hdr =  match str_hdr with
	| DATA (h, data) -> (h, {data with last_flooder=(Some myid)});
	| HELLO _ | RREQ _ | RREP _ -> 
	    failwith "Str_agent#send_waiting_packets" in
      if not (s#forward_data_pkt hdr l3pkt) then 
	Queue.push l3pkt pktq 
    in
    for i = 0 to Queue.length pktq - 1 do
      f (Queue.pop pktq)
    done

      
  method private after_send_any_buffered_pkts dst = 
    (Sched.s())#sched_at ~f:(s#send_waiting_packets ~dst) ~t:Scheduler.ASAP

  method private drop_buffered_packets ~dst = 
    try 
      let q = Hashtbl.find pktqs dst in
      s#log_notice (lazy (sp "Dropping packet(s) for dst %d" dst));      
      while (not (Queue.is_empty q)) do 
	let p = Queue.pop q in
	let str_hdr = L3pkt.str_hdr p in
	match str_hdr with 
	  | DATA (common, _) -> 
	      s#log_info
	      (lazy (sp 
		"Dropped DATA has src %d, dst %d" (L3pkt.l3src p) (L3pkt.l3dst p)));
	      s#log_info
	      (lazy (sp 
		"Dropped DATA has valid entry: sn=%d, dist=%d, age=%.2f"
		common.v_ent.sn common.v_ent.hc common.v_ent.age ));
	      s#log_info
		(lazy (sp 
		  "Dropped DATA has invalid entry: sn=%d, dist=%d, age=%.2f"
		  common.i_ent.sn common.i_ent.hc common.i_ent.age ));
	      s#log_info 
		(lazy (sp 
		  "Dest. %d has %d neighbors, I have %d, our distance is %.2f" (L3pkt.l3dst p)
		  (List.length ((World.w())#neighbors dst))
		  (List.length ((World.w())#neighbors myid))
		  ((World.w())#dist_nodeids dst myid)));
	      stats.S.data_drop <- stats.S.data_drop + 1;
	  | RREP _ | RREQ _ | HELLO _ -> ()
      done;
      
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
	  stats.S.data_drop <- stats.S.data_drop + 1;
	  s#log_notice (lazy (sp "Dropped packet for dst %d" dst));
  )


  method private recv_l3pkt_ ~l3pkt ~sender = (


    let str_hdr = L3pkt.str_hdr l3pkt
    and src = L3pkt.l3src l3pkt 
    and dst = L3pkt.l3dst l3pkt in 
    begin match str_hdr with
      | RREQ _ | RREP _ | HELLO _ -> assert(sender = src)
      | DATA _ -> ()
    end;

    begin match str_hdr with
      | HELLO seqno ->     (* update route to previous hop. *)
	  let nexthop_ent = {age=0.0; sn=seqno; hc=1} in
	  let added = Str_rtab.add_entry rt ~valid:true ~dst:sender ~ent:nexthop_ent
	    ~nh:sender in
	  assert (added);
	  s#after_send_any_buffered_pkts sender;
      | DATA _ -> s#process_data_pkt l3pkt sender;
      | RREQ rreq_hdr -> s#process_rreq_pkt src (L3pkt.l3ttl l3pkt) rreq_hdr
      | RREP rrep_hdr -> s#process_rrep_pkt src (L3pkt.l3ttl l3pkt) rrep_hdr sender;
    end
  )

  (* Entry point for incoming packets. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = 
    s#recv_l3pkt_ ~l3pkt ~sender:l2src



  (* return true if we can answer this rreq, ie if we have a route with lower
     binding cost. 

     assumes that the hopcount field on the rreq *has* been incremented.
  *)
  method private can_reply (str, rreq) = 
    let dst = rreq.rreq_dst in
    if dst = myid then (
      s#log_info 
	(lazy (sp 
	  "Can answer RREQ from originator %d for myself" rreq.rreq_orig));
      true
    ) else (
      let v_ent,_ = Str_rtab.better_valid_route rt ~offset:true str dst in
      let valid_ok = v_ent <> Str_pkt.null_triple in
      if valid_ok then (
	s#log_info 
	(lazy (sp 
	  "Can answer RREQ for dst %d to originator %d (with VALID entry)" rreq.rreq_dst
	  rreq.rreq_orig));
	s#log_info 
	  (lazy (sp 
	    "RREQ has valid entry: sn=%d, dist=%d, age=%.2f"
	    str.v_ent.sn str.v_ent.hc str.v_ent.age ));
	s#log_info 
	  (lazy (sp 
	    "We have valid route with sn=%d, dist=(%d  + rreq_hc=%d), age=%.2f"
	    v_ent.sn v_ent.hc str.orig_hc v_ent.age));
      );
      let i_ent = Str_rtab.better_invalid_route rt metric ~offset:true str dst in
      let invalid_ok = i_ent <> Str_pkt.null_triple in
      if invalid_ok then (
	s#log_info 
	(lazy (sp 
	  "Can answer RREQ for dst %d to originator %d (with INVALID entry)" rreq.rreq_dst
	  rreq.rreq_orig));
	s#log_info 
	  (lazy (sp 
	    "RREQ has invalid entry: age=%.2f, dist=%d, cost=%.2f"
	    str.i_ent.age str.i_ent.hc (Str_rtab.cost metric str.i_ent)));
	  s#log_info 
	    (lazy (sp 
	      "We have invalid route with age=%.2f, dist=(%d  + rreq_hc=%d)"
	      i_ent.age i_ent.hc str.orig_hc));
	  );
      if not (valid_ok || invalid_ok) then (
	s#log_debug (lazy (sp 
	  "Cannot answer RREQ for dst %d to originator %d" rreq.rreq_dst
	  rreq.rreq_orig));
	s#log_info (lazy (sp 
	  "RREQ has valid entry: sn=%d, dist=%d, age=%.2f"
	  str.v_ent.sn str.v_ent.hc str.v_ent.age ));
	s#log_info 
	  (lazy (sp 
	    "Our best valid route has sn=%d, dist=(%d  + rreq_hc=%d + 1), age=%.2f"
	    v_ent.sn v_ent.hc str.orig_hc v_ent.age));
	s#log_info 
	  (lazy (sp 
	    "RREQ has invalid entry: age=%.2f, dist=%d, cost=%.2f"
	    str.i_ent.age str.i_ent.hc (Str_rtab.cost metric str.i_ent)));
	  s#log_info 
	    (lazy (sp 
	      "We have invalid route with age=%.2f, dist=(%d  + rreq_hc=%d + 1)"
	      i_ent.age i_ent.hc str.orig_hc));
      );	
      valid_ok || invalid_ok
    );	
	    
  (* originate route reply message *)
  method private send_rrep (str, rreq)  = 
    let orig = rreq.rreq_orig in 
    let dst = rreq.rreq_dst in
    s#log_info 
      (lazy (sp 
	"Originating RREP for dst %d to originator %d" rreq.rreq_dst orig));
      let rrep =
	if dst = myid then 
	  let v_ent = {age=0.0; sn=seqno; hc=0} 
	  and i_ent = Str_pkt.null_triple in
	  RREP
	    ({orig_hc=0; orig_sn=seqno; v_ent=v_ent; i_ent=i_ent},
	    {rrep_replier=myid; rrep_dst = rreq.rreq_dst; rrep_orig=orig})
	else (
	  let v_ent, _ = Str_rtab.better_valid_route rt ~offset:true str dst in
	  let i_ent = Str_rtab.better_invalid_route rt metric ~offset:true str dst in
	  assert (v_ent <> Str_pkt.null_triple || i_ent <> Str_pkt.null_triple);
	  
	  RREP
	    ({orig_hc=0; orig_sn=seqno; v_ent=v_ent; i_ent=i_ent},
	    {rrep_replier=myid; rrep_dst = rreq.rreq_dst; rrep_orig=orig})
	)
      in
      let ent, nh = Str_rtab.best_valid_entry rt orig in
      if ent = Str_pkt.null_triple then (
	s#log_error (lazy "str_agent.send_rrep"); 
	failwith "str_agent.send_rrep"
      );
      Str_rtab.using_entry rt orig ent;
      let l3pkt = s#make_l3str rrep ~dst:nh in
      stats.S.rrep_orig <- stats.S.rrep_orig + 1;
      s#send_out ~nh l3pkt
	


  method private process_rreq_pkt src ttl (common, rreq) = (

    s#log_info (lazy (sp "Received RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst));

    (* First, create/update reverse path route to originator.*)
    let fresh = Str_rtab.add_entry rt 
      ~valid:true
      ~ent:{age=0.0; sn=common.orig_sn; hc=common.orig_hc + 1}
      ~dst:rreq.rreq_orig
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
      s#log_debug (lazy (sp "Dropping RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst))
    else (
    (* 3. Otherwise, continue RREQ processing. *)
      let common = {common with orig_hc=common.orig_hc + 1} in
      let new_hdr = RREQ (common, rreq) in
      if s#can_reply (common, rreq) then 
	s#send_rrep (common, rreq) 
      else if ttl > 0 then
	let l3pkt = 
	  s#make_l3str ~ttl:(ttl - 1) ~dst:L3pkt.l3_bcast_addr new_hdr in
	s#send_out l3pkt
    )
  )

    
  (* this method assumes that the data_hopcount field of the STR data hdr has
     not yet been incremented. *)
  method private process_data_pkt l3pkt sender = (
    
    let dst = (L3pkt.l3dst l3pkt) and src = (L3pkt.l3src l3pkt) in

    if (L3pkt.l3ttl l3pkt <= 0) then (
      s#log_error (lazy "DATA packet TTL went to 0. looop???");
      failwith "DATA packet TTL went to 0. looop???";
    );
    let common_hdr, data_hdr =  
      match L3pkt.str_hdr l3pkt with
	| DATA (common_hdr, data_hdr) ->     
	    {common_hdr with orig_hc = common_hdr.orig_hc + 1}, data_hdr
	| HELLO _ | RREQ _ | RREP _ -> 
	    failwith "Str_agent#process_data_pkt" in
      let l3pkt = 
	s#make_l3str ~l4pkt:(L3pkt.l4pkt l3pkt) 
	  ~dst ~src ~ttl:(L3pkt.l3ttl l3pkt) 
	  (DATA (common_hdr, data_hdr)) in

      (* Add/update entry to node which originated this packet. *)
      if Str_rtab.add_entry rt ~valid:true ~dst:src ~nh:sender 
	~ent:{age=0.0; sn=common_hdr.orig_sn; hc=common_hdr.orig_hc}
      then
	s#after_send_any_buffered_pkts src;
      
      if (dst = myid) then 
	(* pkt is for us *)
	s#hand_upper_layer ~l3pkt
      else if (s#repairing dst) then 
	s#buffer_packet ~l3pkt
      else if not (s#forward_data_pkt (common_hdr, data_hdr) l3pkt) then (
	s#buffer_packet ~l3pkt; 
	s#init_rreq dst;
      )
  )

  (* Assumes orig_hc on packet has already been incremented *)
  method private forward_data_pkt (common_hdr, data_hdr) l3pkt = 
    (* basic data forwarding as follows:
       - if we have a valid entry respecting invariant w.r.t valid entry on
       data packet,  forward it onto that route.
       - if we have a usable invalid entry respecting invariant w.r.t
       invalid entry on data packet,  forward it onto that route.
       - otherwise, start rreq with valid entry from packet and invalid
       entry being the better of the invalid entry on packet and ours.
    *)
    let dst = L3pkt.l3dst l3pkt and src = L3pkt.l3src l3pkt in
    let (v_ent, v_nh) = Str_rtab.better_valid_route rt common_hdr dst in
    let i_ent = Str_rtab.better_invalid_route rt metric common_hdr dst in
    let (i_u_ent, i_u_nh) = Str_rtab.better_usable_invalid_route rt metric common_hdr dst in
    if v_ent <> Str_pkt.null_triple then (
      let new_common_hdr = 
	if Str_rtab.cost metric i_ent < Str_rtab.cost metric common_hdr.i_ent then
	  {common_hdr with v_ent=v_ent; i_ent=i_ent}
	else
	  {common_hdr with v_ent=v_ent} in
      let new_l3pkt = s#make_l3str ~l4pkt:(L3pkt.l4pkt l3pkt) 
	~dst ~src ~ttl:(L3pkt.l3ttl l3pkt - 1) (DATA (new_common_hdr, data_hdr)) in
      Str_rtab.using_entry rt dst v_ent;
      (* nexthop should be pulled out at same time as i_ent, v_ent in call
	 to str_rtab, but not yet sure what form (tuple?) it will come in*)
      s#send_out ~nh:v_nh new_l3pkt;
      true
    ) else if i_u_ent <> Str_pkt.null_triple then  (
      let new_common_hdr = {common_hdr with i_ent=i_u_ent} in
      let new_l3pkt = s#make_l3str ~l4pkt:(L3pkt.l4pkt l3pkt)
	~dst ~src ~ttl:(L3pkt.l3ttl l3pkt - 1) (DATA (new_common_hdr, data_hdr)) in
      Str_rtab.using_entry rt dst i_u_ent;
      s#send_out ~nh:i_u_nh new_l3pkt;
      true
    ) else (
      s#log_info (lazy (sp "Could not forward data packet to dst %d" dst));
      s#log_info (lazy (sp 
	"DATA pkt has valid entry: sn=%d, dist=%d, age=%.2f"
	common_hdr.v_ent.sn common_hdr.v_ent.hc common_hdr.v_ent.age ));
      s#log_info (lazy (sp 
	"DATA pkt has invalid entry: age=%.2f, dist=%d, cost=%.2f"
	common_hdr.i_ent.age common_hdr.i_ent.hc (Str_rtab.cost metric common_hdr.i_ent)));
      let i_ent, _ = Str_rtab.best_usable_invalid_entry rt metric dst in
      s#log_info (lazy (sp 
	"Our best usable invalid entry: age=%.2f, dist=%d, cost=%.2f, seqno=%d"
	i_ent.age i_ent.hc (Str_rtab.cost metric i_ent) i_ent.sn));
      let i_ent = Str_rtab.best_invalid_entry rt metric dst in
      s#log_info (lazy (sp 
	"Our best invalid entry: age=%.2f, dist=%d, cost=%.2f, seqno=%d"
	i_ent.age i_ent.hc (Str_rtab.cost metric i_ent) i_ent.sn));
      false
    )
    
  method private init_rreq dst = (
    stats.S.rreq_init <-  stats.S.rreq_init + 1;

    let ers_uid = Random.int max_int in (* explained in send_rreq *)
    Hashtbl.replace ers_uids dst ers_uid;

    s#log_info (lazy (sp "Originating RREQ for dst %d" dst));

    s#send_rreq  ~radius:str_TTL_START ~dst ~ers_uid ()
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
	  let repairing = s#repairing dst in 
	  (* need to get repairing *before* buffer data packet, otherwise will
	     always be true *)
	  s#buffer_packet ~l3pkt; 
	  if not repairing then 
	    s#init_rreq dst
      | RREP _ -> stats.S.rrep_xmit <- stats.S.rrep_xmit - 1;
      | HELLO _ | RREQ _ -> raise (Misc.Impossible_Case "Str_agent.mac_callback"); 
	  (* rreqs and hellos are always broadcast *)
    end
    
(* Returns the best valid (resp. invalid) entries from all packets for a given
   destination in our queue, or from the routing table if that one is better. *)
  method private best_waiting_entries dst = 
    let q =  
      try Hashtbl.find pktqs dst 
      with Not_found -> failwith "Str_agent.best_waiting_entries" in 
    let f_valid ent l3pkt = 
      let pkt_ent = match L3pkt.str_hdr l3pkt with 
	| DATA (common_hdr, data_hdr) -> common_hdr.v_ent
	| RREP _ | RREQ _ | HELLO _ -> failwith "Str_agent.best_waiting_entries: not DATA!" in
      if ent >>> pkt_ent then ent else pkt_ent
    and f_invalid ent l3pkt = 
      let pkt_ent = match L3pkt.str_hdr l3pkt with 
	| DATA (common_hdr, data_hdr) -> common_hdr.i_ent
	| RREP _ | RREQ _ | HELLO _ -> failwith "Str_agent.best_waiting_entries: not DATA!" in
      if Str_rtab.cost metric ent < Str_rtab.cost metric pkt_ent then ent else pkt_ent
    in
    let invalid_queue = Queue.fold f_invalid null_triple q in
    let invalid_rtab = Str_rtab.best_invalid_entry rt metric dst in
    let i_ent = if (Str_rtab.cost metric invalid_queue) < (Str_rtab.cost metric invalid_rtab)
    then invalid_queue else invalid_rtab in

    let valid_queue = Queue.fold f_valid null_triple q in
    let valid_rtab, _ = Str_rtab.best_valid_entry rt dst in
    let v_ent = 
      if valid_queue >>> valid_rtab then valid_queue else valid_rtab 
    in v_ent, i_ent

  method private make_l3_rreq_pkt ~radius ~dst = (

    assert (s#repairing dst);
    (* get 'hardest' i_ent and v_ent from waiting packets for dst and rtab, 
       populate RREQ with those. *)
    let v_ent, i_ent = s#best_waiting_entries dst in

    let rreq = RREQ 
      ({orig_hc = 0; orig_sn=seqno; v_ent=v_ent; i_ent=i_ent}, 
      {rreq_orig=myid; rreq_id= rreq_id; rreq_dst=dst; stem_node=myid; stem_distance=0}) in

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
       the check (in send_rreq) for s#repairing.
       But say that we have just started a new rreq phase for this
       destination, and we are currently at ttl 2. Then we would shoot ahead
       with a ttl 32. So we use these per-destination ers_uids which are
       unique across a whole RREQ ERS. 
    *)

    if (s#repairing dst) then
      if beb lsr str_RREQ_RETRIES = 1 then (
	s#log_info (lazy (sp "Reached RREQ_RETRIES for dst %d, abandoning" dst));
	s#drop_buffered_packets ~dst;
      ) else if s#ers_uid dst ers_uid then
	if s#rreq_ratelimit_ok then (
	  
	  (* If we are done repairing this route (maybe a rreq from that node
	     arrived in the meantime) then we would not emit a new RREQ.*)
	  s#log_info (lazy (sp 
	    "ERS RREQ pkt for dst %d with radius %d" dst radius));
	  
	  rreq_id <- rreq_id + 1;
	  
	  let l3pkt = s#make_l3_rreq_pkt ~radius ~dst in
	  
	  (* set up event for next rreq in ERS. *)
	  let next_radius = 
	    if radius <= str_NET_DIAMETER / str_TTL_MULT then
	      radius * str_TTL_MULT else str_NET_DIAMETER in
	  
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
	  s#log_info (lazy (sp 
	    "Delaying RREQ to %d by 0.2 due to rreq ratelimiting" dst));
	  let evt() = s#send_rreq ~beb ~radius ~dst ~ers_uid ()
	  in (Sched.s())#sched_in ~f:evt ~t:0.2;
	)
  )
    

  method private process_rrep_pkt src ttl (common, rrep) sender = (
    
    s#log_info (lazy (sp 
      "Received RREP pkt (replier %d originator %d, dst %d, hc %d)"
      rrep.rrep_replier rrep.rrep_orig rrep.rrep_dst (common.orig_hc + 1)));

    s#log_info (lazy (sp 
      "RREP pkt has valid entry: sn=%d, dist=%d, age=%.2f"
      common.v_ent.sn common.v_ent.hc common.v_ent.age ));
    s#log_info (lazy (sp 
      "RREP pkt has invalid entry: age=%.2f, dist=%d, cost=%.2f"
      common.i_ent.age common.i_ent.hc (Str_rtab.cost metric common.i_ent)));

    
    let (+++) ent offset = {ent with hc = ent.hc + offset} in
    (* Add/update entry to dest for which this rrep advertises reachability.*)
    let updated_v = if common.v_ent <> Str_pkt.null_triple then
      Str_rtab.add_entry rt ~valid:true ~dst:rrep.rrep_dst ~nh:sender 
	~ent:(common.v_ent +++ (common.orig_hc + 1))
    else false
    in
    let updated_i = if common.i_ent <> Str_pkt.null_triple then
      Str_rtab.add_entry rt ~valid:false ~dst:rrep.rrep_dst ~nh:sender 
	~ent:(common.i_ent +++ (common.orig_hc + 1))
    else false 
    in
    let updated = updated_i || updated_v in
    
      if updated then 
      s#after_send_any_buffered_pkts rrep.rrep_dst
      else 
      s#log_info (lazy (sp "Did not update"));

    (* We can forward rrep if we're not the originator, have updated the route
       above, and have a next hop. *)
    if updated && myid <> rrep.rrep_orig then (
      let ent, nh = Str_rtab.best_valid_entry rt rrep.rrep_orig in

      if ent = Str_pkt.null_triple then (
	stats.S.rrep_drop_nohop <- stats.S.rrep_drop_nohop + 1;
	s#log_error 
	(lazy (sp 
	  "Cannot forward RREP because no next hop to originator %d" rrep.rrep_orig));
	s#log_error (lazy ("Entries are "^(Str_rtab.sprint_entries rt rrep.rrep_orig)))
      ) else 
	if ttl > 0 then (
	  Str_rtab.using_entry rt rrep.rrep_orig ent;
	  let new_hdr = Str_pkt.RREP 
	    ({common with orig_hc = common.orig_hc + 1}, rrep) in
	  let l3pkt =  s#make_l3str ~ttl:(ttl-1) ~dst:nh new_hdr in
	  s#send_out ~nh l3pkt
	)
    ) else if not updated && myid <> rrep.rrep_orig then s#log_info (lazy 
      "Cannot forward RREP because did not update route to destination.")
  )


  method private send_out ?(nh=L2pkt.l2_bcast_addr) l3pkt = (
    s#incr_seqno;

    let str_hdr = L3pkt.str_hdr l3pkt in
    let dst = L3pkt.l3dst l3pkt in
    assert (dst <> myid);
    assert (L3pkt.l3ttl l3pkt >= 0);
    
    (* a few sanity checks *)
    begin match str_hdr with
      | RREQ (common, rreq) -> assert (dst = L3pkt.l3_bcast_addr);
	  assert(rreq.rreq_dst <> myid);
      | HELLO _ | DATA _ | RREP _ -> ()
    end;

    stats.S.total_xmit <- stats.S.total_xmit + 1;

    (* for control packets, next hop is equal to src of IP (L3) header. 
       for data packets, next hop is read off of routing table. *)
    match str_hdr with 
      | DATA _ -> 
	  stats.S.data_xmit <- stats.S.data_xmit + 1;
	  s#log_info (lazy 
	    (sp "Forwarding DATA pkt from src %d to dst %d, nexthop %d."
	      (L3pkt.l3src l3pkt) dst nh));
	  s#mac_send_pkt l3pkt nh
      | RREP _ -> 
	  stats.S.rrep_xmit <- stats.S.rrep_xmit + 1;
	  s#mac_send_pkt l3pkt nh
      | RREQ _ -> 
	  stats.S.rreq_xmit <- stats.S.rreq_xmit + 1;
	  assert (Queue.length rreq_times <= str_RREQ_RATELIMIT);
	  Queue.push (Time.time()) rreq_times;
	  if Queue.length rreq_times > str_RREQ_RATELIMIT then 
	    ignore (Queue.pop rreq_times);
	  last_bcast_time <- Time.time(); 
	  s#mac_bcast_pkt l3pkt
      | HELLO _ -> 
	  stats.S.hello_xmit <- stats.S.hello_xmit + 1;
	  last_bcast_time <- Time.time(); 
	  s#mac_bcast_pkt l3pkt
  )


  (* this is a null method because so far we don't need to model apps getting
     packets since we model CBR streams, and mhook catches packets as they enter
     the node *)
  method private hand_upper_layer ~l3pkt = (
    stats.S.data_recv <- stats.S.data_recv + 1;
    s#log_info (lazy (sp "Received app pkt from src %d" (L3pkt.l3src l3pkt)));
  )

  
  method private recv_pkt_app (l4pkt : L4pkt.t) dst = (
    stats.S.data_orig <- stats.S.data_orig + 1;
    s#log_info (lazy (sp "Originating app pkt with dst %d" dst));
    let common_hdr = {
	orig_hc = 0; 
	orig_sn = seqno; 
	v_ent = Str_pkt.null_triple; 
	i_ent = Str_pkt.null_triple; 
    } in
    let data_hdr = {last_flooder = None} in
    let l3pkt = s#make_l3str ~l4pkt ~dst (DATA (common_hdr, data_hdr)) in
    if dst = myid then
      s#hand_upper_layer ~l3pkt
    else 
      s#process_data_pkt l3pkt myid;
  )

  method stats = stats

  method dump_state() = 
    {rt=rt; 
    seqno=seqno; 
    send_hellos=send_hellos;
    stats=stats;
    metric=metric}
  method read_state (s : persist_t) = 
    failwith "pass to initializer"; ()

end




let total_stats ?(stack=0) () = 
  Hashtbl.fold 
    (fun id agent tot -> S.add tot agent#stats)
    agents_array_.(stack)
    (S.create_stats())


open S
let sprint_stats s = 
  let b = Buffer.create 64 in
  let p = sp in 
  Buffer.add_string b "-- Basic Stats:\n";
  Buffer.add_string b (p "   Total xmits: %d\n" s.total_xmit);
  Buffer.add_string b (p "   Delivery ratio: %f (Data orig: %d, Data recv: %d) \n"
    ((float s.data_recv) /. (float s.data_orig )) s.data_orig s.data_recv);
  Buffer.add_string b (p "   Packet xmits per packet delivered: %.2f\n" 
    ((float s.total_xmit) /. (float s.data_recv)));

  Buffer.add_string b "-- Packet Transmission Breakdown:\n";
  Buffer.add_string b (p "   HELLOs: %d, DATA: %d, RREQ: %d, RREP: %d\n"
    s.hello_xmit s.data_xmit s.rreq_xmit s.rrep_xmit);

  Buffer.add_string b "-- Protocol Details:\n";
  Buffer.add_string b (p "   RREQ Init: %d, RREQ Orig: %d, RREP Orig %d\n"
    s.rreq_init s.rreq_orig s.rrep_orig);
  Buffer.add_string b (p "   RREP dropped because no hop to originator: %d\n"
    s.rrep_drop_nohop);
  Buffer.add_string b (p "   DATA dropped: %d\n"  s.data_drop);
  Buffer.contents b


module Persist : Persist.t = struct
  
  type description = int array
      (* array indexed by stack #, contains the number of agents to be read
	 for each stack. *)
      
  let save oc = 
    Log.log#log_notice (lazy "Saving STR agent states..");

    let descr : description = 
      Array.init Node.max_nstacks 
	(fun stack -> Misc.hashlen agents_array_.(stack))
    in
    Marshal.to_channel oc descr [];

    for stack = 0 to Node.max_nstacks - 1 do
      let agents = agents_array_.(stack) in
      Hashtbl.iter (fun nid agent -> 
	Marshal.to_channel oc (nid, agent#dump_state()) []) agents
    done;
    Log.log#log_notice (lazy "Done.")


  let restore ?(verbose=false) ic = 

    Log.log#log_notice (lazy (sp "Restoring STR agent states..."));
    let descr = (Marshal.from_channel ic : description) in

    Array.iteri (fun stack n_agents -> 
      for i = 0 to n_agents - 1 do
	let (nid, state) = 
	  (Marshal.from_channel ic : Common.nodeid_t * persist_t) in
	let node = Nodes.node nid in
	let agent = 
	  (new str_agent ~stack ~state state.metric node) in 
	node#install_rt_agent ~stack ( agent :> Rt_agent.t);
      done;
    ) descr;

    Log.log#log_notice (lazy "Done. (restoring STR agent states)")

end
