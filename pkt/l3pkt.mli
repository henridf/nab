(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* todo: 
   possibly functions should take 'minimal' nec. data structure. 
   For example, set_l3ttl should not take a l3packet but a l3hdr. 
   Otherwise, how does a code segment which only has a l3hdr set the ttl?
*)

open L4pkt

(* ocamldocifying each and every function below is straightforward (if
   long..), not doing until more stability on how this should look *)


val _L3_BCAST_ADDR : int



(** {2 Network Layer (L3) Packet Types} *)

type ease_l3hdr_ext_t = {
  mutable enc_age : Common.time_t;
  mutable anchor_pos : Coord.coordf_t;
  mutable search_dist : float;
}

type grep_flags_t =
    NOT_GREP
  | EASE
  | GREP_DATA
  | GREP_RREQ
  | GREP_RREP
  | GREP_RERR
  | GREP_RADV

type grep_l3hdr_ext_t = {
  mutable grep_flags : grep_flags_t;
  grep_sseqno : int;
  mutable grep_shopcount : int;
}


type l3hdr_ext_t = 
    [ `NONE
    | `EASE_L3HDR_EXT of ease_l3hdr_ext_t (* for now, fresh uses same thing *)
    | `GREP_L3HDR_EXT of grep_l3hdr_ext_t
    ]

(** A l3hdr contains a src, dst, ttl and maybe some protocol-specific
   extensions *)
type l3hdr_t = {
  src : Common.nodeid_t;
  dst : Common.nodeid_t;
  mutable ttl : int;
  ext : l3hdr_ext_t
} 

(** A l3packet_t contains a l3hdr and a l4pkt *)
type l3packet_t 


(** {2 Network Layer (L3) Packet Constructors} *)

val make_l3hdr :
  srcid:Common.nodeid_t ->
  dstid:Common.nodeid_t ->
  ?ttl:int ->
  ?ext:l3hdr_ext_t -> unit -> 
  l3hdr_t


val make_grep_l3hdr_ext :
  flags:grep_flags_t ->
  sseqno:int -> 
  shopcount:int -> 
  l3hdr_ext_t

val make_ease_l3hdr_ext : 
  enc_age:Common.time_t -> 
  anchor_pos:Coord.coordf_t -> 
  l3hdr_ext_t

val make_app_pkt : l3hdr:l3hdr_t -> l3packet_t
val make_l3pkt : l3hdr:l3hdr_t -> l4pkt:l4pkt_t -> l3packet_t
val make_bler_l3pkt :
  srcid:Common.nodeid_t -> dstid:Common.nodeid_t -> l3packet_t
val make_ease_l3pkt :
  srcid:Common.nodeid_t ->
  dstid:Common.nodeid_t ->
  anchor_pos:Coord.coordf_t -> enc_age:Common.time_t -> l3packet_t
val make_dsdv_l3pkt :
  srcid:Common.nodeid_t ->
  ttl:int ->
  originator:Common.nodeid_t -> seqno:int -> nhops:int -> l3packet_t
val make_grep_rreq_l3pkt :
  l3hdr:l3hdr_t -> rreq_payload:grep_rreq_payload_t -> l3packet_t
val make_grep_rrep_l3pkt :
  l3hdr:l3hdr_t -> rrep_payload:grep_adv_payload_t -> l3packet_t
val make_grep_rerr_l3pkt :
  l3hdr:l3hdr_t -> rerr_payload:grep_adv_payload_t -> l3packet_t

(** {2 Network Layer (L3) Packet Deconstructors} *)

val l4pkt : l3pkt:l3packet_t -> l4pkt_t
val l3hdr : l3pkt:l3packet_t -> l3hdr_t
val l3src : l3pkt:l3packet_t -> Common.nodeid_t
val l3dst : l3pkt:l3packet_t -> Common.nodeid_t
val l3ttl : l3pkt:l3packet_t -> int

val l3grepflags : l3pkt:l3packet_t -> grep_flags_t
val l3sseqno : l3pkt:l3packet_t -> int
val l3shopcount : l3pkt:l3packet_t -> int

val l3anchor : l3pkt:l3packet_t ->  Coord.coordf_t
val l3enc_age : l3pkt:l3packet_t ->  Common.time_t
val l3search_dist : l3pkt:l3packet_t ->  float

val dsdv_pkt : pkt:l3packet_t -> dsdv_payload_t
val grep_rrep_pkt : l3pkt:l3packet_t -> grep_adv_payload_t
val grep_rerr_pkt : l3pkt:l3packet_t -> grep_adv_payload_t
val grep_rreq_pkt : l3pkt:l3packet_t -> grep_rreq_payload_t




(** {2 L3 Header Manipulators} *)
val incr_shopcount_pkt : l3pkt:l3packet_t -> unit


val l3pkt_size : l3pkt:l3packet_t -> int
val clone_l3pkt : l3pkt:l3packet_t -> l3packet_t

val set_l3ttl : l3pkt:l3packet_t -> ttl:int -> unit
val decr_l3ttl : l3pkt:l3packet_t -> unit

val succ_dsdv_pkt : pkt:l3packet_t -> src:Common.nodeid_t -> l3packet_t

val set_search_dist : l3hdr:l3hdr_t -> float -> unit
val set_enc_age : l3hdr:l3hdr_t -> Common.time_t -> unit
val set_anchor_pos : l3hdr:l3hdr_t -> Coord.coordf_t -> unit

