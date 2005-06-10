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







(** Functional n-ary trees. *)


exception Empty_error
exception Duplicate_node

(*
  would this be good to disallow Empty in the trees?

  type 'a subtree = Node of 'a * 'a t list
  type 'a tree = Empty | Tree of 'a subtree
*)

type 'a t = Empty | Node of 'a * 'a t list

val map : f:('a -> 'b) -> 'a t -> 'b  t

val belongs : 'a -> 'a t -> bool


val depth :  'a t -> int
  (** Returns the depth of the tree. 
    @raise Empty_error if the tree is empty.
  *)

val root : 'a t -> 'a
  (** Returns the node at the root of the tree. 
    @raise Empty_error if the tree is empty.
  *)

val size : 'a t -> int
  (** Returns the number of (non Empty) nodes in the tree *)

val iter : f:('a -> unit) -> 'a t -> unit
  (** Iterate over tree, presenting each node once to the provided function. 
    Does nothing if the tree has size 0. Order is not specified. *)

val iter2 : f:(parent:'a -> child:'a -> unit) -> 'a t -> unit
  (** Iterate over tree, presenting each node along with its parent to the
    provided function. Does nothing if the tree has size <= 1.
    Order is not specified. *)

val addnode : parent:'a -> node:'a -> 'a t -> 'a t 
  (** [NaryTree.addnode ~parent ~node tree] returns a new tree which is the
    result of adding [node] under [parent] in [tree]. 
    @raise Duplicate_node if [node] is already in [tree].*)

val sprintf : f:('a -> string) -> 'a t -> string
