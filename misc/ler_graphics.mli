open Ler_utils
open Coord
open Graph

val init_gfx : unit -> unit
val close_gfx : unit -> unit
val clear_gfx : unit -> unit

val scale_pos : coordi_t -> coordi_t   (* convert 'absolute' coordinates to graphic coordinates *)
val unscale_pos : coordi_t -> coordi_t (* convert graphic coordinates to 'absolute' coordinates *)
val scale_posf : coordf_t -> coordi_t   (* convert 'absolute' coordinates to graphic coordinates *)
val unscale_posf : coordf_t -> coordi_t (* convert graphic coordinates to 'absolute' coordinates *)


val draw_grid : int -> unit 
val ler_draw_segment : coordi_t array -> unit
val ler_draw_segmentf : (coordf_t) array -> unit
val ler_draw_segments : coordi_t array -> unit
val ler_draw_segments_reflect : coordi_t array -> unit

val draw_nodes : coordi_t array -> unit
val label_nodes : coordi_t array -> unit
val draw_and_label_nodes : coordi_t array -> unit
val circle_nodes : coordi_t array -> float -> unit
val draw_cross : coordi_t -> int -> unit

val draw_route : coordi_t array  -> unit
val animate_route : coordi_t array -> (coordi_t -> 'a) -> unit

val draw_gradient : (coordf_t) array array -> unit

(*val mouse_choose_node : string -> int*)

val dump_window : string -> unit

