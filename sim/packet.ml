(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



(* IMPORTANT: WHEN CHANGING *ANY* HEADER/PACKET FIELDS, 
   MAKE SURE THAT ALL THE APPROPRIATE *_size FUNCTIONS REMAIN VALID *) 

open Misc

let pkt_uid = ref 0

let _L3_BCAST_ADDR = (* 255.255.255.255 *)
  255 + 
  powi ~num:2 ~exp:8 + 
  powi ~num:2 ~exp:16 + 
  powi ~num:2 ~exp:24


let _ADDR_SIZE = 4
let _TTL_SIZE = 1
let _SEQNO_SIZE = 4


(* L4 (APPLICATION) STUFF *)

type bler_payload_t = 
  | ANCH_REQ of (Common.nodeid_t * Common.time_t)  (* dst, current encounter age *)
  | ANCH_RPLY of (Common.nodeid_t * Common.time_t) (* anchorid, encounter age *)

type dsdv_payload_t = {
  originator: Common.nodeid_t;
  seqno: int;
  nhops: int
}

type grep_adv_payload_t = {
  (* adjust l4pkt_size if this changes *)
  adv_dst: Common.nodeid_t;
  adv_seqno: int;
  adv_hopcount: int;
}

let grep_adv_payload_size = 
  _ADDR_SIZE +      (* dst *)
  _SEQNO_SIZE +     (* seqno *)
  _TTL_SIZE         (* hopcount *)

let make_grep_adv_payload  ~adv_dst ~adv_seqno ~adv_hopcount = {
  adv_dst=adv_dst;
  adv_seqno=adv_seqno;
  adv_hopcount=adv_hopcount
}
  
type grep_rreq_payload_t = {
  (* adjust l4pkt_size if this changes *)
  mutable rreq_dst: Common.nodeid_t;
  mutable dseqno: int;    
  mutable dhopcount: int; 
}
let grep_rreq_payload_size = 
  _ADDR_SIZE + 
  _SEQNO_SIZE +      (* dseqno *)
  _TTL_SIZE          (* dhopcount *)

let make_grep_rreq_payload ~rreq_dst ~dseqno ~dhopcount = {
  rreq_dst=rreq_dst;
  dseqno=dseqno;
  dhopcount=dhopcount
}

type l4pld_t = 
    (* if any l4 payload becomes mutable, need to 
       change clone_l4pkt below *)
  | APP_PLD 
  | BLER_PLD of bler_payload_t
  | DSDV_PLD of dsdv_payload_t
  | GREP_RREP_PLD of grep_adv_payload_t
  | GREP_RERR_PLD of grep_adv_payload_t
  | GREP_RREQ_PLD of grep_rreq_payload_t
      
let clone_l4pkt ~l4pkt = l4pkt

let l4pkt_size ~l4pkt = 
  match l4pkt with
    | APP_PLD -> 1500
    | BLER_PLD p  -> raise Misc.Not_Implemented
    | DSDV_PLD p -> raise Misc.Not_Implemented
    | GREP_RREP_PLD p
      -> grep_adv_payload_size
    | GREP_RERR_PLD p
      -> grep_adv_payload_size
    | GREP_RREQ_PLD p
      -> grep_rreq_payload_size
	
(* L3 STUFF *)
type grep_flags_t = 
    NOT_GREP | GREP_DATA | GREP_RREQ | GREP_RREP | GREP_RERR | GREP_RADV


(* note on TTL:
   a ttl is decremented before sending a packet. 
   if decrementing makes it -1 then packet is dropped.
   for example, a 1-hop broadcast has ttl 1, which is decremented to 0 before
   being sent, so that receiving nodes will not forward it further *)

type l3hdr_t = {(* adjust l3hdr_size if this changes *)
  src : Common.nodeid_t;
  (* mutable src_port: Common.port_t;*)
  dst : Common.nodeid_t;
  (* mutable dst_port: Common.port_t;*)
  mutable ttl : int;
  (* Fields used by GREP routing *)
  mutable grep_flags : grep_flags_t;
  grep_sseqno : int;   (* seqno of the source at the time the source sent it *)
  mutable grep_shopcount: int; (* hops traversed since leaving the source *)
}



let clone_l3hdr ~l3hdr = {l3hdr with src = l3hdr.src}
let l3hdr_size ~l3hdr = 
  2 * _ADDR_SIZE (* src, dst *)
  + _TTL_SIZE    (* ttl *)
  + 1           (* grep_flags *) 
  + match l3hdr.grep_flags with
    | NOT_GREP -> 0
    | other -> 
	_SEQNO_SIZE  (* sseqno *)
	+ _TTL_SIZE  (* grep_hopcount *)
	

	
type l3packet_t = {
  l3hdr : l3hdr_t;
  l4pkt : l4pld_t
}

let l3pkt_size ~l3pkt = 
  l3hdr_size ~l3hdr:l3pkt.l3hdr +
  l4pkt_size ~l4pkt:l3pkt.l4pkt

let clone_l3pkt ~l3pkt = {
  l3hdr=clone_l3hdr ~l3hdr:l3pkt.l3hdr;
  l4pkt=clone_l4pkt ~l4pkt:l3pkt.l4pkt;
}

let get_l3hdr ~(l3pkt:l3packet_t) = l3pkt.l3hdr
let get_l3src ~(l3pkt:l3packet_t) = (get_l3hdr l3pkt).src
let get_l3dst ~(l3pkt:l3packet_t) = (get_l3hdr l3pkt).dst
let get_l3ttl ~(l3pkt:l3packet_t) = (get_l3hdr l3pkt).ttl
let set_l3ttl ~(l3pkt:l3packet_t) ~ttl = (get_l3hdr l3pkt).ttl <- ttl

let decr_l3ttl ~(l3pkt:l3packet_t) = 
  l3pkt.l3hdr.ttl <- l3pkt.l3hdr.ttl - 1


let check_grep (l3pkt:l3packet_t) = 
  if (get_l3hdr l3pkt).grep_flags = NOT_GREP then
    raise (Failure "Packet.check_grep")

let get_l3grepflags ~(l3pkt:l3packet_t) = (
  check_grep l3pkt;
  (get_l3hdr l3pkt).grep_flags
)
let get_l3sseqno ~(l3pkt:l3packet_t) = (
  check_grep l3pkt;
  (get_l3hdr l3pkt).grep_sseqno
)
let get_l3shopcount ~(l3pkt:l3packet_t) = (
  check_grep l3pkt;
  (get_l3hdr l3pkt).grep_shopcount
)

let make_l3hdr ~srcid ~dstid ?(ttl=255) () = {
  src=srcid; 
  dst=dstid;
  ttl=ttl;
  grep_flags=NOT_GREP;
  grep_sseqno=0;
  grep_shopcount=0;
}

let make_grep_l3hdr 
  ~srcid 
  ~dstid 
  ~flags 
  ~sseqno 
  ~shopcount
  ?(ttl=255) ()  = {
  src=srcid; 
  dst=dstid;
  ttl=ttl;
  grep_flags=flags;
  grep_sseqno=sseqno;
  grep_shopcount=shopcount;
}

let make_app_pkt ~l3hdr = {
  l3hdr=l3hdr;
  l4pkt=APP_PLD
}

let make_l3_pkt ~l3hdr ~l4pkt = {
  l3hdr=l3hdr;
  l4pkt=l4pkt
}

let make_bler_l3pkt ~srcid ~dstid  = {
  l3hdr=(make_l3hdr ~srcid:srcid ~dstid:dstid ());
  l4pkt=BLER_PLD (ANCH_REQ (dstid, max_float))(*dst, current enc. age*)
}

let make_dsdv_l3pkt ~srcid ~ttl ~originator ~seqno ~nhops = {
  l3hdr=(make_l3hdr ~srcid:srcid ~dstid:Common.nid_bcast ~ttl:ttl ());
  l4pkt=(DSDV_PLD {originator=originator; seqno=seqno; nhops=nhops})
}

let make_grep_rreq_l3pkt ~l3hdr ~rreq_payload = {
  l3hdr = l3hdr;
  l4pkt = GREP_RREQ_PLD rreq_payload
}

let make_grep_rrep_l3pkt ~l3hdr ~rrep_payload = {
  l3hdr = l3hdr;
  l4pkt = GREP_RREP_PLD rrep_payload
}

let make_grep_rerr_l3pkt ~l3hdr ~rerr_payload = {
  l3hdr = l3hdr;
  l4pkt = GREP_RERR_PLD rerr_payload
}

let get_dsdv_pld ~pkt = 
  match pkt.l4pkt with
    | DSDV_PLD p -> p
    | _ -> raise (Failure "Packet.get_dsdv_pld")

let get_grep_rrep_pld ~l3pkt = 
  match l3pkt.l4pkt with
    | GREP_RREP_PLD p -> p
    | _ -> raise (Failure "Packet.get_grep_rrep_pld")

let get_grep_rerr_pld ~l3pkt = 
  match l3pkt.l4pkt with
    | GREP_RERR_PLD p -> p
    | _ -> raise (Failure "Packet.get_grep_rerr_pld")

let get_grep_rreq_pld ~l3pkt = 
  match l3pkt.l4pkt with
    | GREP_RREQ_PLD p -> p
    | _ -> raise (Failure "Packet.get_grep_rreq_pld")

let succ_dsdv_pkt ~pkt ~src = (
  let pld = (get_dsdv_pld pkt) in 
  let l3h = (get_l3hdr pkt) in {
    l3hdr={l3h with ttl = (l3h.ttl - 1); src=src};
    l4pkt=(DSDV_PLD {pld with nhops=((pld.nhops) + 1)})
  }
)

(* L2 STUFF *)

type l2_dst_t = L2_BCAST | L2_DST of Common.nodeid_t

type l2hdr_t = {
  l2src: Common.nodeid_t;
  l2dst: l2_dst_t
}

(* 100 should vaguely represent the mac-layer framing bytes *) 
let l2hdr_size = 100 + 2 * _ADDR_SIZE 

let clone_l2hdr ~l2hdr = {l2hdr with l2src = l2hdr.l2src}

type l2packet_t = {
  l2hdr : l2hdr_t;
  l3pkt : l3packet_t
}

let l2pkt_size ~l2pkt = 
  l2hdr_size +
  l3pkt_size ~l3pkt:l2pkt.l3pkt

let clone_l2pkt ~l2pkt = {
  l2hdr=clone_l2hdr ~l2hdr:l2pkt.l2hdr;
  l3pkt=clone_l3pkt ~l3pkt:l2pkt.l3pkt;
}

let get_l3pkt ~(l2pkt:l2packet_t) = l2pkt.l3pkt

let get_l2hdr ~(pkt:l2packet_t) = pkt.l2hdr
let get_l2src ~(pkt:l2packet_t) = (get_l2hdr pkt).l2src
let get_l2dst ~(pkt:l2packet_t) = (get_l2hdr pkt).l2dst

let make_l2pkt ~srcid ~l2_dst ~l3pkt = 
  let l2hdr = {l2src=srcid; l2dst=l2_dst} 
  in {l2hdr=l2hdr; l3pkt=l3pkt}


