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
  Helper functions for efficient search of geographic neighbors.
  This is _not_ a 'protocol' search - this is just the back-end simulator 
  infrastructure for figuring out which nodes are neighbors.

  @author Henri Dubois-Ferriere.
*)


(**

  Returns a list of the bottom-left coords of all world squares that are
  touched by the circle.
  worldsize_x : width of world in meters
  worldsize_y : height of world in meters
  boxsize : size of one grid square in meters
*)
val xsect_grid_and_circle : 
  center:Coord.coordf_t -> 
  radius:float -> 
  world_size_x:float ->
  world_size_y:float -> 
  tile_size_x:float ->
  tile_size_y:float ->
  grid_size_x:int ->
  grid_size_y:int ->
  Coord.coordi_t list 

