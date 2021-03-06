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

class type waypoint_view =
object
  method getnewpos : gran:float -> float * float
end  

val getmob : Common.nodeid_t -> waypoint_view

val make_uniwaypoint : ?gran:float -> #Node.node -> unit
  (** Uniform waypoint mobility class: waypoints are chosen at random uniformly
    over surface. *)
  
val make_borderwaypoint : ?gran:float -> #Node.node -> unit
  (** Border waypoint mobility class: waypoints are chosen only on the borders. *)

module Persist : Persist.t

