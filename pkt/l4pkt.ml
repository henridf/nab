(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

let _ADDR_SIZE = 4
let _TTL_SIZE = 1
let _SEQNO_SIZE = 4
let _FLOAT_SIZE = 8


(* L4 (APPLICATION) STUFF *)

type hello_payload_t =  Coord.coordf_t
let hello_payload_size = 2 * _FLOAT_SIZE

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

type l4pkt_t = 
    (* if any l4 payload becomes mutable, need to 
       change clone_l4pkt below *)
    [ `NONE
    | `APP_PKT
    | `HELLO_PKT of hello_payload_t
    | `BLER_PKT of bler_payload_t
    | `DSDV_PKT of dsdv_payload_t
    | `GREP_RREP_PKT of grep_adv_payload_t
    | `GREP_RERR_PKT of grep_adv_payload_t
    | `GREP_RREQ_PKT of grep_rreq_payload_t
    ]
      
let clone_l4pkt ~l4pkt = l4pkt

let l4pkt_size ~l4pkt = 
  match l4pkt with
    | `APP_PKT -> 1500
    | `NONE -> 0
    | `HELLO_PKT p -> hello_payload_size
    | `BLER_PKT p  -> raise Misc.Not_Implemented
    | `DSDV_PKT p -> raise Misc.Not_Implemented
    | `GREP_RREP_PKT p
      -> grep_adv_payload_size
    | `GREP_RERR_PKT p
      -> grep_adv_payload_size
    | `GREP_RREQ_PKT p
      -> grep_rreq_payload_size
	
