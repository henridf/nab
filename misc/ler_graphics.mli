open Coord

val init_gfx : unit -> unit
val close_gfx : unit -> unit
val clear_gfx : unit -> unit

val scale_pos : coordf_t -> coordi_t    (* convert 'absolute' coordinates to graphic coordinates *)
val unscale_pos : coordi_t -> coordf_t  (* convert graphic coordinates to 'absolute' coordinates *)

val draw_grid : int -> unit 
val ler_draw_segment : coordf_t array -> unit
val ler_draw_segments : coordf_t array -> unit
val ler_draw_segments_reflect : coordf_t array -> unit

val draw_nodes : coordf_t array -> unit
val label_node : coordf_t -> string -> unit
val draw_and_label_nodes : coordf_t array -> unit
val circle_nodes : coordf_t array -> float -> unit
val draw_cross : coordf_t -> int -> unit

val draw_route : Coord.coordf_t Route.t  -> unit

val draw_gradient : (coordf_t) array array -> unit

val mouse_choose_node :  (coordf_t -> int list) -> string -> int

val dump_window : string -> unit

