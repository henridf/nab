(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

val init : unit -> unit

val redraw : 
  'a -> bool

val clear : 
  unit -> unit

val draw_node :  
  Coord.coordi_t -> 
  unit

val draw_nodes : 
  Coord.coordi_t list 
  -> unit

val draw_segments : 
  (Coord.coordi_t * Coord.coordi_t)  list 
  -> unit

