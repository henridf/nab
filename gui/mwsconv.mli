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







(** 
  Conversions/interface between mws and gui. 
  In particular, between mws coordinates (meters) and screen coordinates (pixels).
  @author Henri Dubois-Ferriere.
*)

val init : unit -> unit

val x_mtr_to_pix : float -> int

val pos_pix_to_mtr : Coord.coordi_t -> Coord.coordf_t
val pos_mtr_to_pix : Coord.coordf_t -> Coord.coordi_t

val route_mtr_to_pix :
  (Coord.coordf_t, 'b) Route.t  ->
  (Coord.coordi_t, 'b) Route.t  

val route_nodeid_to_pix :
  (Common.nodeid_t, 'b) Route.t  ->
  (Coord.coordi_t, 'b) Route.t  

val ease_route_mtr_to_pix :
  (Coord.coordf_t, Coord.coordf_t) Route.ease_route_t  ->
  (Coord.coordi_t, Coord.coordi_t) Route.ease_route_t  

val ease_route_nodeid_to_pix :
  (Common.nodeid_t, Common.nodeid_t) Route.ease_route_t  ->
  (Coord.coordi_t, Coord.coordi_t) Route.ease_route_t  


(* gui => mws *)
val closest_node_at :
  Coord.coordi_t ->
    Common.nodeid_t
