(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)







(** High-level GUI drawing operations *)

val draw_ease_route : 
  ?lines:bool ->
  ?anchors:bool ->
  ?disks:bool ->
  ?portion:float ->
  (Coord.coordi_t, Coord.coordi_t) Route.ease_route_t
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

val dialog_pick_node : 
  ?default:Common.nodeid_t ->
  node_picked_cb:(Common.nodeid_t -> unit) ->
  unit ->
  unit
  (** Ask user for a node id via a dialog box. Checks that id is valid before
    accepting. If provided, optional parameter [default] will be displayed in 
    box.*)
    
