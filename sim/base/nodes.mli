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


(** Various iterators to access and perform actions on all node objects. 
  The iterator functions (iter, iteri, map, mapi, fold) behave like the
  Array or List iterators with similar names from the OCaml standard
  libraries.

  @author Henri Dubois-Ferriere.
*)


val node : int -> Node.node
  (** Return node by index *)  

val iter : (Node.node -> unit) -> unit
val iteri : (Common.nodeid_t -> Node.node -> unit) -> unit
val map : (Node.node -> 'a) -> 'a array
val mapi : (Common.nodeid_t -> 'a) -> 'a array
val fold : (Node.node -> 'a -> 'a) -> 'a -> 'a

val set_nodes : Node.node array -> unit

