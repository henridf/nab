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

(* $Id$ *)







(** Low-level interface to some wrappers around GTK functionality.

  @author Henri Dubois-Ferriere.
 *)


val init : unit -> unit

(* use this function to pack things into the main vbox *)
val packer : 
  unit -> 
  GObj.widget -> 
  unit

val set_expose_event_cb : (GdkEvent.Expose.t -> bool) -> unit

(* use this function to set up a call back for 
   button_press event in the main vbox (bitmap) *)
val install_button_press_cb :  
  (GdkEvent.Button.t -> bool) ->   
  GtkSignal.id

val remove_button_press_cb :  GtkSignal.id -> unit

(* use this function to write a txt message in msg area *)
val txt_msg : string  -> unit

val draw : 
  clear:bool 
  -> unit 
  -> unit


val draw_node :  
  ?col:GDraw.color ->          
  ?target:bool ->
  Coord.coordi_t -> 
  unit

val draw_cross :
  ?diag:bool ->
  ?col:GDraw.color ->          
  ?target:bool ->
  Coord.coordi_t -> 
  unit


val draw_nodes : 
  Coord.coordi_t list 
  -> unit

val draw_segments : 
  ?col:GDraw.color ->          
  ?thick:int ->          
  (Coord.coordi_t * Coord.coordi_t)  list 
  -> unit

val draw_segments_taur : 
  ?col:GDraw.color ->          
  ?thick:int ->          
  (Coord.coordi_t * Coord.coordi_t)  list 
  -> unit

val draw_segments_buf : 
  ?col:GDraw.color ->
  ?thick:int ->          
  (Coord.coordi_t * Coord.coordi_t)  list 
  -> unit

val draw_circle : 
  centr:Coord.coordi_t -> 
  radius:int
  -> unit
