(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Header *)



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
val circle_nodes : ?fill:bool -> coordf_t array -> float -> unit
val draw_cross : coordf_t -> int -> unit

(*val draw_route : color:bool -> route:Coord.coordf_t Route.t  -> unit
val draw_route : color:(hop:int -> routelength:int -> Graphics.color) -> route:Coord.coordf_t Route.t  -> unit*)
val hop_col_color : hop:int -> Graphics.color

val disc_draw_gradient : (coordf_t) array array -> unit
val cont_draw_gradient : (coordf_t * coordf_t) array -> unit

val mouse_choose_node :  (unitpos:coordf_t -> Common.nodeid_t) -> string -> Common.nodeid_t

val dump_window : string -> unit

