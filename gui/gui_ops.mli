(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** High-level GUI drawing operations *)

val draw_ease_route : 
  ?lines:bool ->
  ?anchors:bool ->
  ?disks:bool ->
  ?portion:float ->
  Coord.coordi_t Route.ease_route_t
  -> unit
  (** Draw ease route. 
    Optional arguments [lines] [anchors] [disks] specify respectively which
    information to represent. Default to [true].
    Optional argument [portion] (default 1.0) can indicate that only a first
    fraction of the route is to be drawn.
  *)

val draw_grep_route : 
  Coord.coordi_t Route.grep_route_t
  -> unit
  (** Draw grep route.  *)

val draw_tree :   ?col:GDraw.color ->          
  Coord.coordi_t NaryTree.t -> unit
  (** Draw a tree. *)


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

val user_pick_node : 
  ?msg:string ->
  node_picked_cb:(Common.nodeid_t -> unit)
  -> unit
  -> unit
