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
  ssn : int;         (* Source Seqno: All *)
  dsn : int;         (* Destination Seqno: RREQ *)
  mutable shc : int; (* Source hopcount: All *)
  mutable dhc : int; (* Destination hopcount: RREQ *)
  osrc : Common.nodeid_t; (* OBO Source: RREP *)
  osn : int;              (* OBO Seqno: RREP *)
  ohc : int;      (* OBO Hopcount: RREP *)
  rdst : Common.nodeid_t; (* Route request destination : RREQ *)
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

(** A t contains a l3hdr and a l4pkt *)
type t 


(** {2 Network Layer (L3) Packet Constructors} *)

val make_l3hdr :
  srcid:Common.nodeid_t ->
  dstid:Common.nodeid_t ->
  ?ttl:int ->
  ?ext:l3hdr_ext_t -> unit -> 
  l3hdr_t


val make_grep_l3hdr_ext :
  ?dhc:int ->
  ?dsn:int ->
  ?osrc:Common.nodeid_t ->
  ?ohc:int ->
  ?osn:int ->
  ?rdst:Common.nodeid_t ->
  flags:grep_flags_t ->
  ssn:int -> 
  shc:int -> 
  unit ->
  l3hdr_ext_t

val make_ease_l3hdr_ext : 
  enc_age:Common.time_t -> 
  anchor_pos:Coord.coordf_t -> 
  l3hdr_ext_t

val make_app_pkt : l3hdr:l3hdr_t -> t
val make_l3pkt : l3hdr:l3hdr_t -> l4pkt:L4pkt.t -> t

val make_bler_l3pkt :
  srcid:Common.nodeid_t -> dstid:Common.nodeid_t -> t

val make_ease_l3pkt :
  srcid:Common.nodeid_t ->
  dstid:Common.nodeid_t ->
  anchor_pos:Coord.coordf_t -> 
  enc_age:Common.time_t -> 
  l4pkt:L4pkt.t ->
  t

val make_dsdv_l3pkt :
  srcid:Common.nodeid_t ->
  ttl:int ->
  originator:Common.nodeid_t -> seqno:int -> nhops:int -> t

(** {2 Network Layer (L3) Packet Deconstructors} *)

val l4pkt : l3pkt:t -> L4pkt.t
val l3hdr : l3pkt:t -> l3hdr_t
val l3src : l3pkt:t -> Common.nodeid_t
val l3dst : l3pkt:t -> Common.nodeid_t
val l3ttl : l3pkt:t -> int

val l3grepflags : l3pkt:t -> grep_flags_t
val ssn : l3pkt:t -> int
val shc : l3pkt:t -> int
val dsn : l3pkt:t -> int
val dhc : l3pkt:t -> int
val osrc : l3pkt:t -> Common.nodeid_t
val ohc : l3pkt:t -> int
val osn : l3pkt:t -> int
val rdst : l3pkt:t -> Common.nodeid_t


val l3anchor : l3pkt:t ->  Coord.coordf_t
val l3enc_age : l3pkt:t ->  Common.time_t
val l3search_dist : l3pkt:t ->  float

val dsdv_pkt : pkt:t -> dsdv_payload_t




(** {2 L3 Header Manipulators} *)
val incr_shc_pkt : l3pkt:t -> unit
val decr_shc_pkt : l3pkt:t -> unit


val l3pkt_size : l3pkt:t -> int
val clone_l3pkt : l3pkt:t -> t

val set_l3ttl : l3pkt:t -> ttl:int -> unit
val decr_l3ttl : l3pkt:t -> unit

val succ_dsdv_pkt : pkt:t -> src:Common.nodeid_t -> t

val set_search_dist : l3hdr:l3hdr_t -> float -> unit
val set_enc_age : l3hdr:l3hdr_t -> Common.time_t -> unit
val set_anchor_pos : l3hdr:l3hdr_t -> Coord.coordf_t -> unit

