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


(** Data structure for representing a flood as a tree. 
  @author Henri Dubois-Ferriere.
*)

open Common



type t = Common.nodeid_t NaryTree.t
    (** The type of a flood tree. *)

val create : Common.nodeid_t -> t
  (** Create a flood tree with specified node as originator of flood. *)

val addnode : 
  parent:Common.nodeid_t ->
  node:Common.nodeid_t ->
  t -> t
  (** Add a node to the flood tree. *)

val to_coords : t -> Coord.coordf_t NaryTree.t
  (** Convert a tree of nodes into a tree of positions.*)
