



(** Data structures for representing floods (for
  monitoring or gui purposes). 
  @author Henri Dubois-Ferriere.
*)

open Common

(** Tree-based representation of a flood *)


type t = Common.nodeid_t NaryTree.t

val create : Common.nodeid_t -> t
val addnode : 
  parent:Common.nodeid_t ->
  node:Common.nodeid_t ->
  t -> t

val to_coords : t -> Coord.coordf_t NaryTree.t
     
