(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(* draw node at position of some time in the past *)
val draw_node_time : 
  Common.nodeid_t -> 
  Common.time_t ->
  unit

(* draw nodes at position of some time in the past *)
val draw_nodes_time : 
  Common.nodeid_t list -> 
  Common.time_t ->
  unit


val draw_route : 
  Coord.coordi_t Route.t
  -> unit

(* draw node at current time's position *)
val draw_node :  
  Common.nodeid_t -> 
  unit

(* draw nodes at current time's position *)
val draw_nodes : 
  Common.nodeid_t list 
  -> unit

val draw_all_nodes : 
  unit ->
  unit


val draw_all_boxes : 
  unit ->
  unit

val draw_all_routes : 
  unit ->
  unit
