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


(** Various iterators to perform actions on all node objects. 
  The iterator functions (iter, iteri, map, mapi, fold) behave like the
  Array or List iterators with similar names from the OCaml standard
  libraries.

  Also offers a function to access node objects. 
  @author Henri Dubois-Ferriere.
*)


val node : int -> Simplenode.simplenode
  (** Return node by index *)  

val iter : (Simplenode.simplenode -> unit) -> unit
val iteri : (Common.nodeid_t -> Simplenode.simplenode -> unit) -> unit
val map : (Simplenode.simplenode -> 'a) -> 'a array
val mapi : (Common.nodeid_t -> 'a) -> 'a array
val fold : (Simplenode.simplenode -> 'a -> 'a) -> 'a -> 'a

val set_nodes : Simplenode.simplenode array -> unit

val gpsnode : int -> Gpsnode.gpsnode
  
val gpsiter : (Gpsnode.gpsnode -> unit) -> unit
val gpsiteri : (Common.nodeid_t -> Gpsnode.gpsnode -> unit) -> unit 
val gpsmap : (Gpsnode.gpsnode -> 'a) -> 'a array
val gpsmapi : (Common.nodeid_t -> 'a) -> 'a array
val gpsfold : (Gpsnode.gpsnode -> 'a -> 'a) -> 'a -> 'a

val set_gpsnodes : Gpsnode.gpsnode array -> unit


