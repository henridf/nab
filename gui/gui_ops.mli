(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** High-level GUI drawing operations *)

val draw_route : 
  ?lines:bool ->
  ?anchors:bool ->
  ?disks:bool ->
  portion:float ->
  Coord.coordi_t Route.t
  -> unit

(* draw node at current time's position *)
val draw_node :  
  ?emphasize:bool ->
  Common.nodeid_t -> 
  unit

(* draw nodes at current time's position *)
val draw_nodes : 
  Common.nodeid_t list 
  -> unit

(* connect pairs of nodes *)
val connect_nodes : 
  ?col:GDraw.color ->          
  (Common.nodeid_t * Common.nodeid_t)  list 
  -> unit

(* draw connectivity mesh *)
val draw_connectivity : 
  unit -> unit

val draw_all_nodes : 
  unit ->
  unit

val draw_all_boxes : 
  unit ->
  unit

val draw_all_routes : 
  unit ->
  unit
