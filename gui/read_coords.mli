(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Parsing the epfl 'concentration point graph'.
  @author Henri Dubois-Ferriere.
*)


val g : unit -> string Graph.Graph.t
val box_centeri : int -> Coord.coordf_t
