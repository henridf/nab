(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)
open Packet
open Printf
open Misc

class type grep_agent_t =
  object
    val mutable objdescr : string
    val owner : Node.node_t
    val rtab : Rtab.rtab_t
    val mutable seqno : int
    method private app_send : Packet.l4pld_t -> dst:Common.nodeid_t -> unit
    method private buffer_packet : l3pkt:Packet.l3packet_t -> unit
    method private hand_upper_layer : unit -> unit
    method private incr_seqno : unit -> unit
    method private inv_packet_upwards :
      nexthop:Common.nodeid_t -> l3pkt:Packet.l3packet_t -> unit
    method private inv_ttl_zero : l3pkt:Packet.l3packet_t -> unit
    method newadv : dst:Common.nodeid_t -> rtent:Rtab.rtab_entry_t -> bool
    method objdescr : string
    method private packet_fresh : l3pkt:Packet.l3packet_t -> bool
    method private packets_waiting : dst:Common.nodeid_t -> bool
    method private process_data_pkt : l3pkt:Packet.l3packet_t -> unit
    method private process_radv_pkt :
      l3pkt:Packet.l3packet_t -> sender:Common.nodeid_t -> unit
    method private process_rrep_pkt :
      l3pkt:Packet.l3packet_t -> sender:Common.nodeid_t -> unit
    method private process_rreq_pkt : l3pkt:Packet.l3packet_t -> unit
    method private radv_newadv :
      adv:Packet.grep_adv_payload_t ->
      sender:Common.nodeid_t -> shopcount:int -> bool
    method private recv_l2pkt_hook : Packet.l2packet_t -> unit
    method private send_out : l3pkt:Packet.l3packet_t -> unit
    method private send_rrep : dst:Common.nodeid_t -> obo:Common.nodeid_t -> unit
    method private send_rreq :
      ttl:int -> dst:Common.nodeid_t -> dseqno:int -> dhopcount:int -> unit
    method private send_waiting_packets : dst:Common.nodeid_t -> unit
  end


exception Send_Out_Failure


let agents_array = ref ([||]:grep_agent_t array)

let set_agents arr = agents_array := arr
let agent i = !agents_array.(i)

let incr_shopcount_pkt ~l3pkt  = 
  let l3hdr = Packet.get_l3hdr l3pkt in
  l3hdr.Packet.grep_shopcount <- l3hdr.Packet.grep_shopcount + 1

let set_ttl_pkt ~l3pkt ~ttl = (
  assert (ttl >= 0);
  let l3hdr = Packet.get_l3hdr l3pkt in
  l3hdr.Packet.ttl <- ttl;
)


let set_grepflag_pkt ~l3pkt ~flag = (
  let l3hdr = Packet.get_l3hdr l3pkt in
  l3hdr.Packet.grep_flags <- flag;
)

let _ERS_START_TTL = 2
let _ERS_MULT_FACT = 2


class grep_agent owner : grep_agent_t = 
object(s)

  inherit Log.loggable

  val owner:Node.node_t = owner
  val rtab = Rtab.create ~size:(Param.get Params.nodes) 
  val mutable seqno = 0

  initializer (
    objdescr <- (owner#objdescr ^  "/GREP_Agent");
    owner#add_recv_l2pkt_hook ~hook:s#recv_l2pkt_hook;
    owner#add_app_send_pkt_hook ~hook:s#app_send;
    (*    owner#add_control_hook ~hook:s#ctrl_hook*)
  )

  method private incr_seqno() = (
    seqno <- seqno + 1;
    let update = 
      Rtab.newadv 
	~rt:rtab 
	~dst:owner#id
	~rtent:{
	  Rtab.seqno = Some seqno;
	  Rtab.hopcount = Some 0;
	  Rtab.nexthop = Some owner#id}
    in 
    assert(update);
  )

  method private packets_waiting ~dst = 
    false &&
    raise Misc.Not_Implemented

  method private send_waiting_packets ~dst = 
    if (true) then
      raise Misc.Not_Implemented
    else ()

  (* used for DATA packets which failed to send, to keep them
   in store for when a route is found *)
  method private buffer_packet ~(l3pkt:Packet.l3packet_t) = 
    if (true) then
      raise Misc.Not_Implemented
    else ()



  (* wrapper around Rtab.newadv which additionally checks for 
     open rreqs to that dest and cancels if any,
     buffered packets to that dest and sends them if any *)
  method newadv  
    ~(dst:Common.nodeid_t)
    ~(rtent:Rtab.rtab_entry_t) = (
      let update = (Rtab.newadv ~rt:rtab ~dst:dst ~rtent:rtent)
      in
      if update then (
	(* if route to dst was accepted, send any packets that were waiting
	   for a route to this dst *)
	if (s#packets_waiting ~dst:dst) then (
	  s#send_waiting_packets ~dst:dst
	)
      );
      update
    )

  (* wrapper around Grep_agent.newadv for those that come in 
     radv/rrep packets *)
  method private radv_newadv 
    ~(adv:Packet.grep_adv_payload_t)
    ~(sender:Common.nodeid_t) 
    ~(shopcount:int) = (
      s#newadv
      ~dst:(adv.Packet.adv_dst)
      ~rtent:{
	Rtab.seqno = Some (adv.Packet.adv_seqno);
	Rtab.hopcount = Some ((adv.Packet.adv_hopcount) + shopcount);
	Rtab.nexthop = Some sender 
      }
    )

  (* as in paper *)
  method private packet_fresh ~l3pkt = (
    let pkt_sseqno = Packet.get_l3sseqno ~l3pkt:l3pkt in
    match (Rtab.seqno ~rt:rtab ~dst:(Packet.get_l3src l3pkt)) with
      | None -> true 
      | Some s when (pkt_sseqno > s) -> true
      | Some s when (pkt_sseqno = s) -> 
	  Packet.get_l3shopcount l3pkt 
	  <
	  o2v (Rtab.hopcount ~rt:rtab ~dst:(Packet.get_l3src l3pkt))
      | Some s when (pkt_sseqno < s) -> false
      | _ -> raise (Misc.Impossible_Case "Grep_agent.packet_fresh()")
  )
    
   
  (* as recv_packet in paper *)
  method private recv_l2pkt_hook l2pkt = (

    let l3pkt = Packet.get_l3pkt ~l2pkt:l2pkt in

    (* create or update 1-hop route to previous hop *)
    let sender = Packet.get_l2src l2pkt in
    let sender_seqno = 
      match Rtab.seqno ~rt:rtab ~dst:sender with
	| None -> 1
	| Some n -> n + 1
    in
    let update =  
      s#newadv 
	~dst:sender
	~rtent:{
	  Rtab.seqno = Some sender_seqno;
	  Rtab.hopcount = Some 1;
	  Rtab.nexthop = Some sender
	} in
    assert (update);
    
    (* update route to source if packet came over fresher route than what we
       have *)
    let update =  
      s#newadv 
	~dst:(Packet.get_l3src ~l3pkt:l3pkt)
	~rtent:{
	  Rtab.seqno = Some (Packet.get_l3sseqno ~l3pkt:l3pkt);
	  Rtab.hopcount = Some (Packet.get_l3shopcount ~l3pkt:l3pkt);
	  Rtab.nexthop = Some sender
	} in
    assert (update = (s#packet_fresh ~l3pkt:l3pkt));
    
    (* hand off to per-type method private *)
    begin match Packet.get_l3grepflags ~l3pkt:l3pkt with
      | Packet.GREP_DATA -> s#process_data_pkt ~l3pkt:l3pkt;
      | Packet.GREP_RREQ -> s#process_rreq_pkt ~l3pkt:l3pkt;
      | Packet.GREP_RADV -> s#process_radv_pkt ~l3pkt:l3pkt ~sender:sender;
      | Packet.GREP_RREP -> s#process_rrep_pkt ~l3pkt:l3pkt ~sender:sender;
      | Packet.NOT_GREP -> raise (Failure "Grep_agent.recv_l2pkt_hook");
    end
  ) 

  method private process_radv_pkt ~l3pkt ~sender = 
    raise Misc.Not_Implemented

  method private process_rreq_pkt ~l3pkt = (
    let rreq = (Packet.get_grep_rreq_pld ~l3pkt:l3pkt) in

    let answer_rreq = 
      (rreq.Packet.rreq_dst = owner#id)
      ||
      begin match (Rtab.seqno ~rt:rtab ~dst:rreq.Packet.rreq_dst) with 
	| None -> false
	| Some s when (s > rreq.Packet.dseqno) -> true
	| Some s when (s = rreq.Packet.dseqno) ->
	    (o2v (Rtab.hopcount ~rt:rtab ~dst:rreq.Packet.rreq_dst) =
	      rreq.Packet.dhopcount)
	| Some s when (s < rreq.Packet.dseqno) -> false
	| _ -> raise (Misc.Impossible_Case "Grep_agent.answer_rreq()") end
    in

    if (answer_rreq) then 
      s#send_rrep 
	~dst:(Packet.get_l3src ~l3pkt:l3pkt) 
	~obo:rreq.Packet.rreq_dst
     else 
      s#send_out ~l3pkt:l3pkt
  )
      
  method private send_rrep ~dst ~obo = (

    (* this will be checked for in send_out, but we have to check it
       here so that the o2v below is safe *)
    begin match (Rtab.nexthop ~rt:rtab ~dst:dst) with
      | None -> raise (Failure "Grep_agent.send_rrep: had no route")
      | Some nh -> () end;
    

    let adv = Packet.make_grep_adv_payload 
      ~adv_dst:obo
      ~adv_seqno:(o2v (Rtab.seqno ~rt:rtab ~dst:obo))
      ~adv_hopcount:(o2v (Rtab.hopcount ~rt:rtab ~dst:obo))
    in
    s#send_out 
      ~l3pkt:(
	Packet.make_grep_rrep_l3pkt 
	~rrep_payload:adv
	~l3hdr:(
	  Packet.make_grep_l3hdr
	  ~srcid:owner#id
	  ~dstid:dst
	  ~flags:Packet.GREP_RREP
	  ~sseqno:seqno
	  ~shopcount:0
	  ~ttl:(o2v (Rtab.hopcount ~rt:rtab ~dst:obo))
	  ()
	)
      )
  )

  method private inv_packet_upwards ~nexthop ~l3pkt = (
    (* this expects to be called just prior to sending l3pkt
       and so assumes that ttl has already been decremented on l3pkt *)

    let agent_nexthop = agent nexthop in
    assert (
      (agent_nexthop#newadv
	~dst:(Packet.get_l3dst ~l3pkt:l3pkt)
	~rtent:{
	  Rtab.seqno = Some (Packet.get_l3sseqno ~l3pkt:l3pkt);
	  Rtab.hopcount = Some ((Packet.get_l3ttl ~l3pkt:l3pkt) + 1);
	  Rtab.nexthop = Some 0 (* this rtent will be ignored anyway *)
	}) = false
    )
  )
    
  method private inv_ttl_zero ~l3pkt = (
    assert ((Packet.get_l3ttl l3pkt)  = 0);
  )
    
  method private process_data_pkt 
    ~(l3pkt:Packet.l3packet_t) = 
    (

      if ((Packet.get_l3dst ~l3pkt:l3pkt) = owner#id) then (   (* for us *)
	begin match Packet.get_l3grepflags ~l3pkt:l3pkt with
	  | Packet.GREP_DATA | Packet.GREP_RREP -> s#inv_ttl_zero ~l3pkt:l3pkt
	  | _ -> raise (Misc.Impossible_Case "Grep_agent.process_data_pkt");
	end;
	s#hand_upper_layer();
      ) else (
	
	let dst = (Packet.get_l3dst ~l3pkt:l3pkt) in
	try 
	  s#send_out ~l3pkt:l3pkt
	with 
	  | Send_Out_Failure -> 
	      begin
		(* important to buffer packet first because send_rreq checks for
		   this *)
		s#buffer_packet ~l3pkt:l3pkt;
		let (dseqno,dhopcount) = 
		  begin match (Rtab.seqno ~rt:rtab ~dst:dst) with
		    | None -> (0, max_int)
		    | Some s -> (s, o2v (Rtab.hopcount ~rt:rtab ~dst:dst)) end
		in
		s#send_rreq 
		  ~ttl:_ERS_START_TTL 
		  ~dst:dst 
		  ~dseqno:dseqno
		  ~dhopcount:dhopcount;
	      end
      )
    )

  method private send_rreq ~ttl ~dst ~dseqno ~dhopcount = (

    if (s#packets_waiting ~dst:dst) then (
      (* we check this as a simple way to not do a repeat rreq from a 
	 previous rreq timeout. Ie, if a rrep came in in the meantime, then we
	 sent all packets, and don't need to send a new rreq. 
	 At some point a more detailed implementation would probably need a
	 separate representation of pending rreqs to know which have been
	 satisfied, etc *)
      let l3hdr = 
	Packet.make_grep_l3hdr
	  ~srcid:owner#id
	  ~dstid:Packet._L3_BCAST_ADDR
	  ~flags:Packet.GREP_RREQ
	  ~sseqno:seqno
	  ~shopcount:0
	  ~ttl:ttl 
	  ()
      in
      let rreq_payload = 
	Packet.make_grep_rreq_payload
	  ~rreq_dst:dst
	  ~dseqno:dseqno 
	  ~dhopcount:dhopcount
      in
      let l3pkt = 
	Packet.make_grep_rreq_l3pkt 
	  ~l3hdr:l3hdr
	  ~rreq_payload:rreq_payload
      in
      let next_rreq_event() = 
	(s#send_rreq 
	  ~ttl:(ttl*_ERS_MULT_FACT)
	  ~dst:dst
	  ~dseqno:dseqno
	  ~dhopcount:dhopcount)
      in	
      (* we say that maximum 1-hop traversal is 20ms, 
	 ie half of value used by AODV. Another difference relative to AODV
	 is that we use ttl, not (ttl + 2).
	 This is ok while we use a simple MAC, and ok since our AODV impl 
	 will use the same values*)
	 
      let timeout = Sched.Time ((i2f ttl) *. 0.02) in
      (Gsched.sched())#sched_at ~handler:next_rreq_event ~t:timeout;
      s#send_out ~l3pkt:l3pkt

    )
  )
    

  method private process_rrep_pkt 
    ~(l3pkt:Packet.l3packet_t) 
    ~(sender:Common.nodeid_t) = (
      
      let adv = (Packet.get_grep_rrep_pld ~l3pkt:l3pkt)
      in 
      let update = (s#radv_newadv 
	~adv:adv 
	~sender:sender 
	~shopcount:(Packet.get_l3shopcount ~l3pkt:l3pkt))
      in 
      if ((Packet.get_l3dst ~l3pkt:l3pkt) != owner#id) then
	s#send_out ~l3pkt:l3pkt
      else ()
    )
    
  method private send_out ~l3pkt = (
    
    let dst = Packet.get_l3dst ~l3pkt:l3pkt in
    assert (dst != owner#id);
    assert (Packet.get_l3ttl ~l3pkt:l3pkt >= 0);
    assert (Packet.get_l3sseqno ~l3pkt:l3pkt >= 1);

    decr_l3ttl ~l3pkt:l3pkt;
    if ((Packet.get_l3ttl ~l3pkt:l3pkt) < 0) then (
      s#log_info (sprintf "Dropping packet (negative ttl)");
      
      assert(
	Packet.get_l3grepflags ~l3pkt:l3pkt = Packet.GREP_RADV ||
	Packet.get_l3grepflags ~l3pkt:l3pkt = Packet.GREP_RREQ);
    ) else (
      
    s#incr_seqno();
    incr_shopcount_pkt ~l3pkt:l3pkt;

    begin match (Packet.get_l3grepflags ~l3pkt:l3pkt) with

      | Packet.NOT_GREP -> 
	  raise (Failure "Grep_agent.send_out")
	  
      | Packet.GREP_RADV 
      | Packet.GREP_RREQ -> 
	  assert (dst = Packet._L3_BCAST_ADDR);
	  owner#mac_bcast_pkt ~l3pkt:l3pkt;
	  
      | Packet.GREP_DATA 
      | Packet.GREP_RREP ->
	  let nexthop = 
	    match Rtab.nexthop ~rt:rtab ~dst:dst  with
	      | None -> raise (Failure "Grep_agent.send_out: had no route")
	      | Some nh -> nh
	  in 
	  s#inv_packet_upwards ~nexthop:nexthop ~l3pkt:l3pkt;
	  try 
	    owner#mac_send_pkt ~l3pkt:l3pkt ~dstid:nexthop;
	  with Simplenode.Mac_Send_Failure ->
 	    raise Send_Out_Failure
    end
    )
  )
		
	

  (* this is a null method because so far we don't need to model apps getting
     packets since we model CBR streams, and mhook catches packets as they enter
     the node *)
  method private hand_upper_layer() = ()

  (*
    method ctrl_hook action = (

    s#log_debug (sprintf "Originating dsdv (ttl 5) ");

    let pkt = 
      Packet.DSDV_PKT (Packet.make_dsdv_pkt 
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
    s#log_info (sprintf "%d received app pkt with dst %d"
      owner#id dst);

  )





end
