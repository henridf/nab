(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Diffusion packet types and manipulators.
  @author Henri Dubois-Ferriere.
*)


(* L3 STUFF *)
type diff_flags_t = 
    DIFF_DATA | DIFF_RADV

type t = {
  mutable diff_flags : diff_flags_t;
  ssn : int;         (* Source Seqno: All *)
  mutable shc : int; (* Source hopcount: All *)
}

val hdr_size : t -> int

val clone : t -> t

val flags : t -> diff_flags_t
val ssn : t -> int
val shc : t -> int


val incr_shc_pkt : t -> unit
val decr_shc_pkt : t -> unit

val make_diff_hdr :
  flags:diff_flags_t ->
  ssn:int -> 
  shc:int -> 
  t
  
