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







(** Functional n-ary trees. *)


(*
  would this be good to disallow Empty in the trees?

  type 'a subtree = Node of 'a * 'a t list
  type 'a tree = Empty | Tree of 'a subtree
*)

type 'a t = Empty | Node of 'a * 'a t list

val map : f:('a -> 'b) -> 'a t -> 'b  t

val belongs : 'a -> 'a t -> bool
val height :  'a t -> int

val root : 'a t -> 'a
  (** Returns the node at the root of the tree. Raise [Failure "root"] if tree
    is empty.*)

val size : 'a t -> int
  (** Returns the number of (non Empty) nodes in the tree *)
val iter : f:('a -> unit) -> 'a t -> unit
  (** Iterate over tree, presenting each node once to the provided function. 
    Order is not specified. *)

val iter2 : f:(parent:'a -> child:'a -> unit) -> 'a t -> unit
  (** Iterate over tree, presenting each node along with its parent to the
    provided function. Order is not specified *)


val addnode : parent:'a -> node:'a -> 'a t -> 'a t 
  (** [NaryTree.addnode ~parent ~node tree] returns a new tree which is the
    result of adding [node] under [parent] in [tree]. Raise [Failure "nth"] if
    [parent] is not in [tree].*)


