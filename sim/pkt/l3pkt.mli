



(** Layer 3 packet types and manipulators.
  A lot of the packet headers are protocol specific (see the corresponding
  files); here we only take care of the generic fields (src, dst, ..) and
  functions.
  
  @author Henri Dubois-Ferriere.
*)


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

type l3hdr_ext_t = 
    [ `NONE
    | `EASE_HDR of Ease_pkt.t 
    | `GREP_HDR of Grep_pkt.t
    | `AODV_HDR of Aodv_pkt.t
    | `DIFF_HDR of Diff_pkt.t
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


val make_app_pkt : l3hdr:l3hdr_t -> t
val make_l3pkt : l3hdr:l3hdr_t -> l4pkt:L4pkt.t -> t

(** {2 Network Layer (L3) Packet Deconstructors} *)

val l4pkt : l3pkt:t -> L4pkt.t
val l3hdr : l3pkt:t -> l3hdr_t
val l3src : l3pkt:t -> Common.nodeid_t
val l3dst : l3pkt:t -> Common.nodeid_t
val l3ttl : l3pkt:t -> int




val ease_hdr : t -> Ease_pkt.t
val grep_hdr : t -> Grep_pkt.t
val aodv_hdr : t -> Aodv_pkt.t
val diff_hdr : t -> Diff_pkt.t


(** {2 L3 Header Manipulators} *)


val l3pkt_size : l3pkt:t -> int
val clone_l3pkt : l3pkt:t -> t

val set_l3ttl : l3pkt:t -> ttl:int -> unit
val decr_l3ttl : l3pkt:t -> unit

