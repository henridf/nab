open Ler_utils

val init_gfx : unit -> unit
val close_gfx : unit -> unit
val clear_gfx : unit -> unit

val scale_pos : int * int -> int * int   (* convert 'absolute' coordinates to graphic coordinates *)
val unscale_pos : int * int -> int * int (* convert graphic coordinates to 'absolute' coordinates *)
val scale_posf : float * float -> int * int   (* convert 'absolute' coordinates to graphic coordinates *)
val unscale_posf : float * float -> int * int (* convert graphic coordinates to 'absolute' coordinates *)


val draw_grid : int -> unit 
val ler_draw_segment : pos_t array -> unit
val ler_draw_segmentf : (float * float) array -> unit
val ler_draw_segments : pos_t array -> unit
val ler_draw_segments_reflect : Ler_utils.pos_t array -> unit

val draw_nodes : (int * int) array -> unit
val label_nodes : (int * int) array -> unit
val draw_and_label_nodes : (int * int) array -> unit
val circle_nodes : (int * int) array -> float -> unit
val draw_cross : int * int -> int -> unit

val draw_route : pos_t array  -> unit
val animate_route : pos_t array -> (int * int -> 'a) -> unit

val draw_gradient : (float * float) array array -> unit

(* change these to use itinerary.toarray_ *)
val animate_itin : Itin.Itinerary.t -> Lattice.Lattice.t -> (int * int -> 'a) -> unit 
val draw_itin : Itin.Itinerary.t -> Lattice.Lattice.t -> unit

(*val mouse_choose_node : string -> int*)

val dump_window : string -> unit

