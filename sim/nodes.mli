(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Various iterators to perform actions on all node objects. 
  The iterator functions (iter, iteri, map, mapi, fold) behave like the
  Array or List iterators with similar names from the OCaml standard
  libraries.

  Also offers a function to access node objects. 
  @author Henri Dubois-Ferriere.
*)


val node : int -> Simplenode.simplenode
  (** Return node by index *)  

val iter : (Simplenode.simplenode -> unit) -> unit
val iteri : (Common.nodeid_t -> unit) -> unit
val map : (Simplenode.simplenode -> 'a) -> 'a array
val mapi : (Common.nodeid_t -> 'a) -> 'a array
val fold : (Simplenode.simplenode -> 'a -> 'a) -> 'a -> 'a

val set_nodes : Simplenode.simplenode array -> unit

val gpsnode : int -> Gpsnode.gpsnode
  
val gpsiter : (Gpsnode.gpsnode -> unit) -> unit
val gpsmap : (Gpsnode.gpsnode -> 'a) -> 'a array
val gpsmapi : (Common.nodeid_t -> 'a) -> 'a array
val gpsfold : (Gpsnode.gpsnode -> 'a -> 'a) -> 'a -> 'a

val set_gpsnodes : Gpsnode.gpsnode array -> unit


