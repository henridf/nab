(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open L3pkt

(** {2 L2 Packet Types} *)

type l2_dst_t = L2_BCAST | L2_DST of Common.nodeid_t
and l2hdr_t = { l2src : Common.nodeid_t; l2dst : l2_dst_t; } 
type t = { l2hdr : l2hdr_t; l3pkt : L3pkt.t; } 


(** {2 L2 Packet Constructors} *)

val clone_l2hdr : l2hdr:l2hdr_t -> l2hdr_t
val make_l2pkt :
  srcid:Common.nodeid_t -> l2_dst:l2_dst_t -> l3pkt:L3pkt.t -> t
val clone_l2pkt : l2pkt:t -> t



(** {2 L2 Packet Deconstructors} *)

val l3pkt : l2pkt:t -> L3pkt.t
val l2hdr : pkt:t -> l2hdr_t
val l2src : pkt:t -> Common.nodeid_t
val l2dst : pkt:t -> l2_dst_t
val string_of_l2dst : l2_dst_t -> string

(** {2 Packet Sizes} *)

val l2hdr_size : int
val l2pkt_size : l2pkt:t -> int
