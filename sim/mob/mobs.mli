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

(** Waypoint over EPFL mobility class. 

  The campus is modeled as a set of 'points of interest' (ie buildings,
  outdoor areas, points along a road). Each point of interest is connected
  to other nearby points, in such a way that the direct path between each   
  point respects the campus toplogy. There are currently ~ 120 points in the
  epfl topology, see gui/epflcoords.ml.
  
  The mobility model can be seen as a 'random waypoint on a graph':
  A node is initially dropped down at random on one of these points of
  interest. It picks at random any other point of interest. Then, the 
  shortest path route is computed to reach the point of interest *via the
  graph of available points*. The node then moves along this path until the
  desired point of interest is reached. This is intended to model the fact  
  that in a real (campus or urban) environment, nodes do not move in a
  straight line toward their target, but of course move along paths and
  around obstacles.      
*)
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
