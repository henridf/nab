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



(** Some simple mobility processes.
  @author Henri Dubois-Ferriere.
*)



(** Uniform waypoint mobility class: waypoints are chosen at random uniformly
  over surface. *)
class uniwaypoint : 
  #Simplenode.simplenode ->  
  ?gran:float ->
  unit ->
  Mob.t

(** Border waypoint mobility class: waypoints are chosen only on the borders. *)
class borderwaypoint :
  #Simplenode.simplenode ->
  ?gran:float ->
  unit ->
  Mob.t

(** Waypoint over EPFL mobility class. *)
class epfl_waypoint :
  #Simplenode.simplenode ->
  unit ->
  Mob.t
  
  
(** Billiard mobility model: 
  Pick a uniform random direction, advance in that direction. A new random
  direction is selected every T seconds, where T is exponentially distributed;
  E(T) is {!Params.x_size}[ * speed ], ie the time it takes to traverse the
  network.
  Boundaries are reflecting, ie a node bounces off as off of a mirror.
*)
class billiard : 
  #Simplenode.simplenode ->
  ?gran:float ->
  unit ->
  Mob.t

(** Discrete Random Walk, moves by 1.0 in one of 4 cardinal directions at each
  step *)
class discreteRandomWalk :
  #Simplenode.simplenode ->
  unit ->
  Mob.t
