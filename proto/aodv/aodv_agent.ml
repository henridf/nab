(*still to do : 
  hello messages
*)
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


(** A simple implementation of the AODV routing protocol.

  This implementation is not yet a literal and exhaustive transcription of the
  RFC, in particular the following functions have yet to be implemented:
  - Features for dealing with unidirectional links (blacklists, 'A' option to
  request a RREP-ACK) are not supported.
  - Actions after reboot.
  - Section 6.10 of the RFC ("Maintaing Local Connectivity") is partially
  implemented.
*)

(* xxx/RFC Questions:
   6.7 : "if needed", create a route entry to previous w/o valid seqno.

   will need to remove and clean up Mac_send_failure. do this in // with
   rethinking llacks design.

   do a stats thing a bit like for MACS. need to think if there is a common,
   basic stats  or not.

   ers_uids cannot be an array, nor can pktqs

*)


(* tests:
   get a mode make a rreq for an inexistant node, check that rreq grows and
   there is beb and MAX_RREQ_LIMITS is respected.
   throw RREQ_RATELIMIT + 1 app packets to a node with no routes and check
   that they don't all go out at once.

   do a random mix of mobility and traffic, and check that ttl drops on data
   packets don't happen (would indicate a loooop)
 *)

open Aodv_grep_common
open Aodv_pkt
open Aodv_defaults
open Printf
open Misc

let agents_array_ = 
  Array.init Simplenode.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))

let make_l3aodv aodvhdr ~src ~dst = 
  L3pkt.make_l3pkt 
    ~l3hdr:(L3pkt.make_l3hdr ~src ~dst ~ext:(`AODV_HDR aodvhdr) ()) ~l4pkt:`EMPTY

class aodv_agent ?(stack=0) ?(localrepair=true) ?(dstonly=false) theowner  = 
object(s)

  inherit Rt_agent_base.base ~stack theowner 

  (* 
   *  Instance Variables. 
   *)

  val mutable myseqno = 0 (* our sequence number. *)

  val mutable last_bcast_time = Time.time()  
    (* We keep track of the last time since we sent any broadcastpacket in
       order to know when hello message is nec. (rfc 6.9)*)

  val default_rreq_flags = {g=true; d=dstonly; u=false}

  val rt = Aodv_rtab.create (Param.get Params.nodes)
    (* our routing table *)

  val pktqs : (Common.nodeid_t, L3pkt.t Queue.t) Hashtbl.t = 
    Hashtbl.create aodv_PKTQUEUE_SIZE

  val ers_uids = Array.create (Param.get Params.nodes) 0
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


  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/AODV_Agent";
    Hashtbl.replace agents_array_.(stack) theowner#id (s :> aodv_agent);
    s#incr_seqno();
    (Sched.s())#sched_in ~f:s#clean_rreq_cache ~t:(aodv_PATH_DISCOVERY_TIME +. 1.);
  )

  (* 
   * Methods
   *)

  method myid = myid

  method private incr_seqno() = myseqno <- myseqno + 1

(* Periodically go through rreq cache and remove rreqs which are too old to be
   of any use, to avoid that it fills up and gets to O(N) size. *)
  method private clean_rreq_cache() = (
    let oldest_acceptable = (Time.time()) -. aodv_PATH_DISCOVERY_TIME in
    let check key time = 
      if time < oldest_acceptable then (
	Hashtbl.remove rreq_cache key;
	assert (Hashtbl.find_all rreq_cache key = [])
      )  in
    Hashtbl.iter check rreq_cache;
    (Sched.s())#sched_in ~f:s#clean_rreq_cache ~t:aodv_PATH_DISCOVERY_TIME;
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
    try 
      let pktqueue = Hashtbl.find pktqs dst in
      begin try 
	while true do
	  let l3pkt = Queue.pop pktqueue in
	  s#log_info 
	    (lazy (sprintf "Sending buffered DATA pkt from src %d to dst %d."
	      (L3pkt.l3src l3pkt) dst));
	  if Aodv_rtab.valid rt dst then
	    Aodv_rtab.set_lifetime rt dst aodv_ACTIVE_ROUTE_TIMEOUT;
	      s#send_out l3pkt;

	  s#send_out l3pkt
	done
      with Queue.Empty -> Hashtbl.remove pktqs dst 
      end;
    with Not_found -> ()

  method private after_send_any_buffered_pkts dst = 
    (Sched.s())#sched_at ~f:(s#send_waiting_packets ~dst) ~t:Scheduler.ASAP

  method private drop_buffered_packets ~dst = 
    Hashtbl.remove pktqs dst


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
	  Grep_hooks.drop_data();
	  s#log_notice (lazy (sprintf "Dropped packet for dst %d" dst));
  )


  method private recv_l3pkt_ ~l3pkt ~sender = (
    
    let aodv_hdr = L3pkt.aodv_hdr l3pkt 
    and src = L3pkt.l3src l3pkt 
    and dst = L3pkt.l3dst l3pkt in 
    if aodv_hdr <> DATA then assert(sender = src);

    begin match aodv_hdr with
      | DATA -> s#process_data_pkt l3pkt sender;
      | RREQ rreq -> s#process_rreq_pkt src (L3pkt.l3ttl l3pkt) rreq
      | RREP rrep -> s#process_rrep_pkt src rrep ;
      | RERR rerr -> s#process_rerr_pkt src rerr ;
      | RREP_ACK -> raise Misc.Not_Implemented;
    end
  ) 

  method mac_recv_l3pkt _  = ()

  (* Entry point for incoming packets. *)
  method mac_recv_l2pkt l2pkt = (
    
    (* xxx/rfc not sure what to do with l2src. 
       Ie, check if we have a route to that src and if so update lifetime? *)
    s#recv_l3pkt_ ~l3pkt:(L2pkt.l3pkt l2pkt) ~sender:(L2pkt.l2src l2pkt)
  )


  (* return true if we can answer this rreq, as per rfc 6.6 *)
  method private can_reply rreq = 
    if rreq.rreq_dst = myid  then true else
      match Aodv_rtab.seqno rt rreq.rreq_dst, rreq.rreq_flags.d with 
	| Some sn, true -> 
	    Aodv_rtab.valid rt rreq.rreq_dst 
	    && (sn >=  rreq.rreq_dst_sn || rreq.rreq_flags.u)
	| _ -> false

	    
  (* originate route reply message *)
  method private send_rrep ~dst ~orig ~hc ~dst_sn lifetime = 
    match Aodv_rtab.nexthop_maybe rt orig with
	Some nh -> 
	  let rrep = Aodv_pkt.make_rrep_hdr ~hc ~dst ~dst_sn ~orig ~lifetime () in
	  let l3pkt = make_l3aodv rrep ~src:myid ~dst:nh in
	  s#send_out l3pkt
      | None -> 
	  s#log_error (lazy "aodv_agent.send_rrep"); 
	  failwith "aodv_agent.send_rrep"


  (* called when we have a route request which we can reply to. *)
  method private reply_rreq rreq = (
    
    (* Possibly increment our seqno if we are the dst of the rreq (6.6.1) *)
    if rreq.rreq_dst = myid && rreq.rreq_dst_sn = myseqno + 1 then
      s#incr_seqno();

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
    if rreq.rreq_dst <> myid && rreq.rreq_flags.g then
      let grat_lifetime = Aodv_rtab.lifetime rt rreq.rreq_orig in
      s#send_rrep ~hc:hop_count ~dst:rreq.rreq_orig ~dst_sn:rreq.rreq_orig_sn
	~orig:rreq.rreq_dst grat_lifetime
  )


  method private process_rreq_pkt src ttl rreq = (
    (* Follows the steps from rfc 6.5 *)

    s#log_info (lazy (sprintf "Received RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst));

    (* 1. Update route to previous hop.*)
    Aodv_rtab.add_entry_neighbor rt src;
    s#after_send_any_buffered_pkts src;

    (* 2. If we have received this RREQ recently, discard and don't do
       anything else. *)
    let discard = try 
      let last_time = Hashtbl.find rreq_cache (rreq.rreq_orig, rreq.rreq_id) in
      if last_time +. aodv_PATH_DISCOVERY_TIME > Time.time() then true 
      else false
    with Not_found -> false in

    Hashtbl.replace rreq_cache (rreq.rreq_orig, rreq.rreq_id) (Time.time());
    if discard then 
      s#log_info (lazy (sprintf "Dropping RREQ pkt (originator %d), dst %d"
	rreq.rreq_orig rreq.rreq_dst))
    else (
      (* 3. Otherwise, continue RREQ processing. 
	 First, create/update reverse path route to originator.*)
      Aodv_rtab.add_entry_rreq rt rreq src;
      s#after_send_any_buffered_pkts rreq.rreq_orig;
	
      if s#can_reply rreq then s#reply_rreq rreq
      else if ttl > 0 then
	(* xxx check IP spec about ttl - do we emit a packet with ttl 0?*)
	(* Update rreq fields as per rfc 6.5 and rebroadcast. *)
	let seqno = match Aodv_rtab.seqno rt rreq.rreq_dst with
	  | Some s -> max s rreq.rreq_dst_sn
	  | None -> rreq.rreq_dst_sn in
	let new_rreq = RREQ {rreq with 
	  rreq_hopcount = rreq.rreq_hopcount + 1; 
	  rreq_dst_sn = seqno} in
	let l3hdr = L3pkt.make_l3hdr ~src:myid ~dst:L3pkt.l3_bcast_addr
	  ~ttl:(ttl - 1) ~ext:(`AODV_HDR new_rreq) () in
	let l3pkt = L3pkt.make_l3pkt ~l3hdr ~l4pkt:`EMPTY in
	s#send_out l3pkt
    )
  )

      
  (* Case (ii) of rfc 6.11 : received a pkt with dest for which we don't have
     valid route, and are not repairing either. 
     Actions:
     - Increment seqno for dst (NOP if we had no entry for dst)
     - Increase lifetime of route (NOP if we had no entry for dst)
     - Unicast rerr to source, will only have one invalid destination
     
     xxx not really clear from RFC if we should take precursor list into account
     in this situation. assume we don't.
  *)
  method private no_nexthop_rerr ~dst ~sender = (

    assert (not (Aodv_rtab.valid rt dst) && not (Aodv_rtab.repairing rt dst));
    Aodv_rtab.incr_seqno rt dst;

    let flags = {nd=false} 
    and seqno = Aodv_rtab.seqno rt dst in

    Aodv_rtab.set_lifetime rt dst aodv_DELETE_PERIOD;
    
    let aodv_hdr = Aodv_pkt.make_rerr_hdr ~flags [(dst, seqno)] in
    let l3pkt = make_l3aodv aodv_hdr  ~src:myid ~dst:sender in
    s#send_out l3pkt
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
	      if (not (Aodv_rtab.repairing rt dst)) then s#orig_rreq dst;
	  | false, false ->
	      (* some weirdo forwarded us a packet for an unkown dest. *)
	      s#no_nexthop_rerr ~dst ~sender
	  | true, _ -> 
	      (* route is valid, so we can fw data packet. *)
	      L3pkt.decr_l3ttl l3pkt;
	      Aodv_rtab.set_lifetime rt dst aodv_ACTIVE_ROUTE_TIMEOUT;
	      s#send_out l3pkt;
    )

    
  method private orig_rreq dst = (
    s#incr_seqno();
    
    ers_uids.(dst) <- Random.int max_int; (* explained in send_rreq *)
    Aodv_rtab.repair_start rt dst;
    s#log_info (lazy (sprintf "Originating RREQ for dst %d" dst));
    let start_ttl = match Aodv_rtab.hopcount_maybe rt dst with 
      | Some hc -> aodv_TTL_INCREMENT + hc
      | None -> aodv_TTL_START in
    s#send_rreq ~local:false ~radius:start_ttl ~dst ~ers_uid:ers_uids.(dst) ()
  )

  (* Originate local repair as per rfc 6.12 *)
  method private orig_rreq_localrepair ~dst ~src = (
    s#incr_seqno();
    ers_uids.(dst) <- Random.int max_int; (* explained in send_rreq *)
    Aodv_rtab.repair_start rt dst;
    s#log_info (lazy (sprintf "Originating RREQ for dst %d (local repair)" dst));

    let aodv_MIN_REPAIR_TTL = Opt.default 0 (Aodv_rtab.hopcount_maybe rt dst)
    and bw_hops = Opt.default 0 (Aodv_rtab.hopcount_maybe rt src) in
    let start_ttl =  (max aodv_MIN_REPAIR_TTL bw_hops/2) + aodv_LOCAL_ADD_TTL in
    
    s#send_rreq ~local:true ~radius:start_ttl ~dst ~ers_uid:ers_uids.(dst) ()
  )

  method private do_local_repair dst = raise Misc.Not_Implemented

    
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

    if unreachables_with_prec <> [] then (* at least 1 unreachable node has precursor *)
      let new_rerr = 
	Aodv_pkt.make_rerr_hdr ~flags:{nd=nd} 
	  (List.combine unreachables_with_prec ur_wp_seqnos) in

      (* If only 1 precursors for all unreachable nodes, RERR is
	 unicast, else RERR is broadcast. *)
      let dst = if Aodv_rtab.have_many_precursors rt unreachables_with_prec then
	L3pkt.l3_bcast_addr 
      else List.hd (Aodv_rtab.precursors rt
 	(List.find (Aodv_rtab.has_precursors rt) unreachables_with_prec)) in

      (* xxx if unicast, shouldn't we take ttl of incoming RERR - 1 ? *)
      let l3hdr = 
	L3pkt.make_l3hdr ~src:myid ~dst ~ttl:1 ~ext:(`AODV_HDR new_rerr) () in
      let l3pkt = L3pkt.make_l3pkt ~l3hdr ~l4pkt:`EMPTY in
      s#send_out l3pkt
    )




  (* Callback from MAC layer when a unicast packet can not be delivered
     (for those MAC layers which support link-layer acknowledgements). *)
  method packet_fw_failure l3pkt (nexthop : Common.nodeid_t) = 
    begin match L3pkt.aodv_hdr l3pkt with
      | DATA ->     
	  let dst = L3pkt.l3dst l3pkt in
	  if s#do_local_repair dst then  
	    s#orig_rreq_localrepair ~dst ~src:(L3pkt.l3dst l3pkt)
	  else 
	    s#orig_rerr false dst 
      | RREP _ | RREP_ACK | RERR _ | RREQ _ -> ()
    end
    
  method private make_l3_rreq_pkt ~radius ~dst = (
    let flags = 
      {default_rreq_flags with u = ((Aodv_rtab.seqno rt dst) = None)} in
    
    let rreq = make_rreq_hdr ~flags  ~hc:1  ~rreq_id
      ~rreq_dst:dst
      ~rreq_dst_sn:(Opt.default 0 (Aodv_rtab.seqno rt dst))
      ~orig:myid
      ~orig_sn:myseqno () in

    let l3hdr = L3pkt.make_l3hdr ~src:myid ~dst:L3pkt.l3_bcast_addr 
      ~ext:(`AODV_HDR rreq) ~ttl:radius () in
    L3pkt.make_l3pkt ~l3hdr ~l4pkt:`EMPTY 
  )


  (* Return true if we can send out a rreq without breaking RREQ_RATELIMIT
     (ie, can originate max RREQ_RATELIMIT rreqs per second). *)
  method private rreq_ratelimit_ok = 
    Queue.length rreq_times < aodv_RREQ_RATELIMIT ||
    Queue.peek rreq_times < Time.time() -. 1. 

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
    
    rreq_id <- rreq_id + 1;

    if beb lsr aodv_RREQ_RETRIES = 1 then (
      s#log_info (lazy (sprintf "Reached RREQ_RETRIES for dst %d, abandoning" dst));
      s#drop_buffered_packets ~dst;
      Aodv_rtab.repair_end rt dst
    ) else if ers_uids.(dst) = ers_uid && (Aodv_rtab.repairing rt dst) &&
      s#rreq_ratelimit_ok then (
	
	(* If we are done repairing this route (maybe a rreq from that node
	   arrived in the meantime) then we would not emit a new RREQ.*)
	s#log_info (lazy (sprintf 
	  "ERS RREQ pkt for dst %d with radius %d" dst radius));
	
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
	      if next_radius = aodv_NET_DIAMETER then 2 else 0
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

	(* Schedule the event *)
	(Sched.s())#sched_in ~f:evt ~t:timeout;
	(* Send out rreq packet *)
	s#send_out l3pkt;
      ) else (
	(* we cannot send the RREQ immediately, because if so we would not be
	   respecting RREQ_RATELIMIT. So, we will try again in a little bit.*)
	let evt() = s#send_rreq ~local ~beb ~radius ~dst ~ers_uid ()
	in (Sched.s())#sched_in ~f:evt ~t:0.2;
      )
  )
    

  method private process_rrep_pkt src rrep  = (
    (* Follows the steps from rfc 6.7 *)

    s#log_info (lazy (sprintf "Received RREP pkt (originator %d, dst %d)"
	rrep.rrep_orig rrep.rrep_dst));

    (* 1. Update route to previous hop.*)
    Aodv_rtab.add_entry_neighbor rt src;
    s#after_send_any_buffered_pkts src;

    (* 2. Update route to destination of route reply. *)
    let updated = Aodv_rtab.add_entry_rrep rt rrep src in
    s#after_send_any_buffered_pkts rrep.rrep_dst;

    (* 3. We can forward rrep if we're not the originator, have updated the route
       above, and have a next hop. *)
    if updated && myid <> rrep.rrep_orig then 
      match Aodv_rtab.nexthop_maybe rt rrep.rrep_orig with
	| None -> ()
	| Some nh ->
	    Aodv_rtab.add_precursor rt ~dst:rrep.rrep_dst ~pre:nh;
	    (* According to 6.7 update below should be done but seems odd and 
	       inconsistent with what we do for other onehop routes we've created
	       Aodv_rtab.add_precursor rt ~dst:src ~pre:nh; *)

	    (* xxx ttl should be taken from l3pkt of incoming rrep and
	       decremented. *)
	    let new_rrep = Aodv_pkt.incr_rrep_hopcount rrep in
	    let l3pkt = make_l3aodv ~src:myid ~dst:nh (Aodv_pkt.RREP new_rrep) in
	    s#send_out l3pkt
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
  method private process_rerr_pkt src rerr = (
    assert (List.length rerr.unreach >= 1);

    (* Follows the steps from rfc 6.11 *)

    (* 1. Compute list of unreachable destinations 
       (destinations in the RERR for which there exists a corresponding entry
       in the local routing table that has the transmitter of the received
       RERR as the next hop.) *)
    let check_unreach (dst, _) =  Aodv_rtab.nexthop_maybe rt dst = Some src in
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

      (* xxx if unicast, shouldn't we take ttl of incoming RERR - 1 ? *)
      let l3hdr = 
	L3pkt.make_l3hdr ~src:myid ~dst ~ttl:1 ~ext:(`AODV_HDR new_rerr) () in
      let l3pkt = L3pkt.make_l3pkt ~l3hdr ~l4pkt:`EMPTY in
      s#send_out l3pkt
    )


  method private send_out l3pkt = (
    
    let aodv_hdr = L3pkt.aodv_hdr l3pkt in
    let dst = L3pkt.l3dst l3pkt in
    assert (dst <> myid);
    assert (L3pkt.l3ttl l3pkt >= 0);

    (* a few sanity checks *)
    begin match aodv_hdr with
      | RREQ rreq -> assert (dst = L3pkt.l3_bcast_addr); assert(rreq.rreq_dst <> myid);
      | RERR rerr -> assert (List.length rerr.unreach > 0)
      | DATA | RREP_ACK | RREP _ -> ()
    end;

    (* for control packets, next hop is equal to src of IP (L3) header. 
       for data packets, next hop is read off of routing table. *)
    match aodv_hdr with 
      | DATA -> s#mac_send_pkt l3pkt (Aodv_rtab.nexthop rt dst) 
      | RREP _ -> s#mac_send_pkt l3pkt dst 
      | RREQ _ -> 
	  assert (Queue.length rreq_times <= aodv_RREQ_RATELIMIT);
	  Queue.push (Time.time()) rreq_times;
	  if Queue.length rreq_times > aodv_RREQ_RATELIMIT then 
	    ignore (Queue.pop rreq_times);
	  last_bcast_time <- Time.time(); 
	  s#mac_bcast_pkt l3pkt
      | RERR _ -> 
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
    Grep_hooks.recv_data();
    s#log_info (lazy (sprintf "Received app pkt from src %d" (L3pkt.l3src l3pkt)));
  )

  
  method private app_recv_l4pkt (l4pkt : L4pkt.t) dst = (
    assert (dst <> myid);
    Grep_hooks.orig_data();

    s#log_info (lazy (sprintf "Originating app pkt with dst %d" dst));
    let l3hdr = L3pkt.make_l3hdr ~src:myid ~dst () in 
    let l3pkt = (L3pkt.make_l3pkt ~l3hdr ~l4pkt) in
    s#process_data_pkt l3pkt myid;
  )
end
