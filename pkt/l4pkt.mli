(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(** {2 Application Layer (L4) Packet Types} *)

type hello_payload_t = Coord.coordf_t

and bler_payload_t =
    ANCH_REQ of (Common.nodeid_t * Common.time_t)
  | ANCH_RPLY of (Common.nodeid_t * Common.time_t)
and dsdv_payload_t = {
  originator : Common.nodeid_t;
  seqno : int;
  nhops : int;
} 
and grep_adv_payload_t = {
  adv_dst : Common.nodeid_t;
  adv_seqno : int;
  adv_hopcount : int;
} 
and grep_rreq_payload_t = {
  mutable rreq_dst : Common.nodeid_t;
  mutable dseqno : int;
  mutable dhopcount : int;
} 

type l4pkt_t = 
    [ `NONE
    | `APP_PKT
    | `HELLO_PKT of hello_payload_t
    | `BLER_PKT of bler_payload_t
    | `DSDV_PKT of dsdv_payload_t
    | `GREP_RREP_PKT of grep_adv_payload_t
    | `GREP_RERR_PKT of grep_adv_payload_t
    | `GREP_RREQ_PKT of grep_rreq_payload_t
    ]

(** {2 Application Layer (L4) Packet Constructors} *)

val make_grep_adv_payload :
  adv_dst:Common.nodeid_t ->
  adv_seqno:int -> adv_hopcount:int -> grep_adv_payload_t

val make_grep_rreq_payload :
  rreq_dst:Common.nodeid_t ->
  dseqno:int -> dhopcount:int -> grep_rreq_payload_t

val clone_l4pkt : l4pkt:'a -> 'a


(** {2 Application Layer (L4) Packet Manipulators} *)

val l4pkt_size : l4pkt:l4pkt_t -> int
