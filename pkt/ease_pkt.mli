(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** EASE packet types and manipulators.
  @author Henri Dubois-Ferriere.
*)


type t = {
  mutable enc_age : Common.time_t;
  mutable anchor_pos : Coord.coordf_t;
  mutable search_dist : float;
}

val make_ease_hdr : 
  enc_age:Common.time_t -> 
  anchor_pos:Coord.coordf_t -> 
  t

val anchor : t ->  Coord.coordf_t
val enc_age : t ->  Common.time_t
val search_dist : t ->  float

val set_search_dist : t -> float -> unit
val set_enc_age : t -> Common.time_t -> unit
val set_anchor_pos : t -> Coord.coordf_t -> unit

val clone : t -> t
