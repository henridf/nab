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







(** Data structures for representing floods (for
  monitoring or gui purposes). 
  @author Henri Dubois-Ferriere.
*)

open Common

(** Tree-based representation of a flood *)


type t = Common.nodeid_t NaryTree.t

val create : Common.nodeid_t -> t
val addnode : 
  parent:Common.nodeid_t ->
  node:Common.nodeid_t ->
  t -> t

val to_coords : t -> Coord.coordf_t NaryTree.t
     
