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







(** 
  Conversions/interface for data between simulator and gui. 
  In particular, between simulation coordinates (meters) and screen coordinates (pixels).
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

val ler_route_mtr_to_pix :
  (Coord.coordf_t, Coord.coordf_t) Route.ler_route_t  ->
  (Coord.coordi_t, Coord.coordi_t) Route.ler_route_t  

val ler_route_nodeid_to_pix :
  (Common.nodeid_t, Coord.coordf_t) Route.ler_route_t  ->
  (Coord.coordi_t, Coord.coordi_t) Route.ler_route_t  

val closest_node_at :
  Coord.coordi_t ->
    Common.nodeid_t
