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


(** High-level GUI drawing operations *)


val draw_ler_route : 
  ?lines:bool ->
  ?anchors:bool ->
  ?disks:bool ->
  ?portion:float ->
  (Coord.coordi_t, Coord.coordi_t) Ler_route.t
  -> unit
  (** Draw LER route. 
    Optional arguments [lines] [anchors] [disks] specify respectively which
    information to represent. Default to [true].
    Optional argument [portion] (default 1.0) can indicate that only a first
    fraction of the route is to be drawn.
  *)


val draw_grep_route : 
  ?portion:float ->
  Coord.coordi_t Od_route.t ->
  unit
  (** Draw grep route.  
    Optional argument [portion] (default 1.0) can indicate that only a first
    fraction of the route is to be drawn.
  *)

val draw_tree :   
  ?col:GDraw.color ->          
  ?thick:int ->
  Coord.coordi_t NaryTree.t -> unit
  (** Draw a tree with segments of color ~col and thickness ~thick *)

val draw_node :  
  ?emphasize:bool ->
  Common.nodeid_t -> 
  unit
  (** Draw a node. If optional [emphasize] is [true], node is highlighted
    (this is suitable for making the source and destination stand out, for
    example). *)  


val draw_nodes : 
  Common.nodeid_t list 
  -> unit
  (** Draw some nodes *)

val connect_nodes : 
  ?col:GDraw.color ->          
  ?thick:int ->
  (Common.nodeid_t * Common.nodeid_t)  list 
  -> unit
  (** Connect pairs of nodes with a segment of color ~col and
    thickness ~thick *)

val annotate_nodes : 
  (Common.nodeid_t -> string) -> unit
  (** [annotate_nodes f] Annotates each node [nid] on the display with the
    text that is given by calling the provided function with [nid] as
    argument. The function [f] can simply return the empty string for nodes
    that need no annotations. *)



val draw_connectivity : 
  ?col:GDraw.color -> ?thick:int -> unit -> unit
  (** Draw full connectivity mesh using ~col and segment thickness ~thick*)

val draw_all_nodes : 
  ?col:GDraw.color ->
  unit ->
  unit
    (** Draw all nodes. *)
    
val draw_all_nodes_wparam : 
  ?normalize:bool -> 
  ?unit_radio:float ->
  float array ->
  unit
    (** Draw all nodes with an associated parameter per node. For
	each node, it draws a circle centered in the node of size 
	proportional to the value of its parameter. 
	This circle is filled with a color based on the value of the
	parameter.
	Normalize: if true, divide all parameters by the maximum value
	so that the maximum value is one.
	Unit radio: indicates the radio of the circle that correspond
	with the unitary parameter.*)

val user_pick_node : 
  ?msg:string ->
  node_picked_cb:(Common.nodeid_t -> unit)
  -> unit
  -> unit
  (** Ask the user to pick a node by clicking on the drawing area. 
    Once the user has picked, the provided callback [node_picked_cb] is called
    with the chosen nodeid as argument.
  *)

val dialog_pick_node : 
  ?default:Common.nodeid_t ->
  node_picked_cb:(Common.nodeid_t -> unit) ->
  unit ->
  unit
  (** Ask user for a node id via a dialog box. Checks that id is valid before
    accepting. If provided, optional parameter [default] will be displayed in 
    box. Once chosen, the provided callback [node_picked_cb] is called
    with the chosen nodeid as argument.
  *)
  

(**/**)

val draw_all_boxes : 
  unit ->
  unit

val draw_all_routes : 
  unit ->
  unit

