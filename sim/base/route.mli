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



(** Data structures for globally representing routes. These data structures
  can be used e.g. by gui routines to draw a route after it has been computed, or
  for measuring statistical aspect of route(s) obtained.

  The data structure is essentially a list of hops. The notion of 'hop' here
  is polymorphic: each hop can be a node id, a node object, a geographcial
  position, or any other type depending on the particular application. Each
  hop can also have an optional "info" attached, to represent
  protocol-specific information (for example, in a LER/GREASE route, this field
  can be used to represent the search cost and anchor obtained).

  @author Henri Dubois-Ferriere.
*)

open Common



type ('a, 'b) hop_t = {hop:'a; mutable info:'b option}
    (** The polymorphic type representing a hop in a route.
      ['a] could be a {!Common.nodeid_t} or a {!Coord.coordf_t}.
      ['b] is optional, intended to hold protocol-specific information
      associated with a particular hop (such as the current anchor in
      LER/GREASE).
    *)
    (* note: info's get copied around in some functions of this module.
       therefore, caution if creating a new info type with  mutable field(s). *)




type ('a, 'b) t = ('a, 'b) hop_t list
    (** A route is simply a list of hops. 
      A route between src and dest should start at the src and finish at the dest.
      Therefore  its length is 1 more than the number of hops in the route.
    *)    
    (* the type is exposed so that list operations can be easily used on
    routes *)


type 'a hops_only_route_t = ('a, unit) t
    (** A route type for routes with no protocol-specific information. *)


val create : unit -> ('a, 'b) t

val add_hop : ('a, 'b) t -> ('a, 'b) hop_t -> ('a, 'b) t 
  (* Returns a new route with hop at end *)

(*val append_hops : front:'a t -> back:'a t -> 'a t*)
(* append a route to the end of another *)

val nth_hop : ('a, 'b) t -> int -> ('a, 'b) hop_t
val last_hop : ('a, 'b) t -> ('a, 'b) hop_t

val length : ('a, 'b) t -> int


val eucl_length : dist_f:(Coord.coordf_t -> Coord.coordf_t -> float) ->
  (Coord.coordf_t, 'b) t -> float
  (* Compute Euclidean length of route given a distance function *)


val i2c : (Common.nodeid_t, 'b) t -> (Coord.coordf_t, 'b) t


(** Functions for printing various types of routes into strings. *)


val sprintf_hopsonly : (Coord.coordf_t, 'b) t -> string  
val sprinti_hopsonly : (Coord.coordi_t, 'b) t -> string  
val sprintnid_hopsonly : (Common.nodeid_t, 'b) t -> string  


