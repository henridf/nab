open Ler_utils

val init_gfx : unit -> unit
val close_gfx : unit -> unit
val clear_gfx : unit -> unit

val draw_grid : int -> unit 
val ler_draw_segment : pos_t array -> unit
val ler_draw_segments : pos_t array -> unit
val ler_draw_segments_reflect : Ler_utils.pos_t array -> unit

val draw_nodes : (int * int) array -> unit
val label_nodes : (int * int) array -> unit
val draw_and_label_nodes : (int * int) array -> unit
val circle_nodes : (int * int) array -> float -> unit
val draw_cross : int * int -> int -> unit

val animate_itin : Itin.Itinerary.t -> Lattice.Lattice.t -> (int * int -> 'a) -> unit
val draw_itin : Itin.Itinerary.t -> Lattice.Lattice.t -> unit
