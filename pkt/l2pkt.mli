(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open L3pkt

(** {2 MAC Layer (L2) Packet Types} *)

type l2_dst_t = L2_BCAST | L2_DST of Common.nodeid_t
and l2hdr_t = { l2src : Common.nodeid_t; l2dst : l2_dst_t; } 
type l2packet_t = { l2hdr : l2hdr_t; l3pkt : l3packet_t; } 


(** {2 MAC Layer (L2) Packet Constructors} *)

val clone_l2hdr : l2hdr:l2hdr_t -> l2hdr_t
val make_l2pkt :
  srcid:Common.nodeid_t -> l2_dst:l2_dst_t -> l3pkt:l3packet_t -> l2packet_t
val clone_l2pkt : l2pkt:l2packet_t -> l2packet_t

val l2pkt_size : l2pkt:l2packet_t -> int

(** {2 MAC Layer (L2) Packet Deconstructors} *)

val l3pkt : l2pkt:l2packet_t -> l3packet_t
val l2hdr : pkt:l2packet_t -> l2hdr_t
val l2src : pkt:l2packet_t -> Common.nodeid_t
val l2dst : pkt:l2packet_t -> l2_dst_t
val string_of_l2dst : l2_dst_t -> string

val l2hdr_size : int
