(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
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
  (** This initialization function must be called at startup, before calling
    any of nab gui API functions. *)

val hpacker : 
  unit -> 
  GObj.widget -> 
  unit
  (** Use this function to pack any widget created by your application into
    the horizontal box. *)

val vpacker : 
  unit -> 
  GObj.widget -> 
  unit
  (** Use this function to pack any widget created by your application into
    the vertical box. *)

val set_expose_event_cb : (GdkEvent.Expose.t -> bool) -> unit
  (** Use this to provide a callback function that will be called by NAB each
    time the GUI window needs to be redrawn. *)


val install_button_press_cb :  
  (GdkEvent.Button.t -> bool) ->   
  GtkSignal.id
  (** Use this function to set up a callback for 
    button_press event in the main drawing area (bitmap). *)

val remove_button_press_cb :  GtkSignal.id -> unit
  (** Use this function to undo [install_button_press_cb]. *)

val txt_msg : string  -> unit
  (** Display a text message in msg area. *)

val clear : unit -> unit
  (** Clear the drawing area. *)

val draw_node :  
  ?col:GDraw.color ->          
  ?target:bool ->
  Coord.coordi_t -> 
  unit
  (** Draw a node. *)


val draw_cross :
  ?diag:bool ->
  ?col:GDraw.color ->          
  ?target:bool ->
  Coord.coordi_t -> 
  unit
  (** Draw a cross. *)

val draw_segments : 
  ?col:GDraw.color ->          
  ?thick:int ->          
  (Coord.coordi_t * Coord.coordi_t)  list 
  -> unit
  (** Draw some segments. *)

val draw_circle : 
  centr:Coord.coordi_t -> 
  radius:int
  -> unit
  (** Draw a circle. *)

val draw_filled_circle : 
  centr:Coord.coordi_t -> 
  radius:int ->
  line_color:GDraw.color ->          
  fill_color:GDraw.color           
  -> unit
  (** Draw a circle filled with the color specified by ~fill_color *)

(**/**)
val draw_segments_taur : 
  ?col:GDraw.color ->          
  ?thick:int ->          
  (Coord.coordi_t * Coord.coordi_t)  list 
  -> unit

