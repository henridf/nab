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
val make_epfl_waypoint :  #Node.node -> unit

module Persist : Persist.t


