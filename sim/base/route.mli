(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Header *)



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


type 'a ease_info_t = 
    {anchor: 'a; 
    anchor_age: Time.time_t; 
    searchcost: float}
    (**
      The info type attached to hops in a EASE/GREASE route.
      The anchor changes at the hop which does a new anchor search (or in the
      case of GREASE, at the hop which locally has a better anchor, in which
      case the searchcost will be 0.)
      The anchor_age represents the age of the current anchor, and also changes 
      when the anchor changes.
      The searchcost can only be non-zero at hops where the anchor is
      different than the previous hop.
    *)
    
type ('a, 'b) ease_route_t = ('a, 'b ease_info_t) t
    (** A EASE/GREASE route, using [ease_info_t] for additional info. 
      Note this type is still polymorphic in ['a], to allow representing the
      hops either as node ids or as geographical positions (useful in a GUI).*)

type grep_info_t = Flood.t
    (** The optional info type attached to a GREP route is a {!Flood.t}
      object, which represents the (flooded) search which some nodes
      perform. At hops where no flood was necessary, the [hop_t.info] field is
      set to [None]. *)


type 'a grep_route_t = ('a, grep_info_t) t




val create : unit -> ('a, 'b) t

val add_hop : ('a, 'b) t -> ('a, 'b) hop_t -> ('a, 'b) t 
  (* Returns a new route with hop at end *)

(*val append_hops : front:'a t -> back:'a t -> 'a t*)
(* append a route to the end of another *)

val nth_hop : ('a, 'b) t -> int -> ('a, 'b) hop_t
val last_hop : ('a, 'b) t -> ('a, 'b) hop_t

val length : ('a, 'b) t -> int


val ease_route_valid : ('a, 'b) ease_route_t -> src:'a -> dst:'a -> bool
  (* generic checks:
     - length >= 1
     - searchcost >= 0
     - searchcost can only be non-zero when anchor changes
     - anchor_age must be monotonically decreasing
     - anchor_age can only change when anchor changes
     - starts at src, ends at dst
     - is loop-free (no hop is repeated twice)
  *)

val search_cost : ('a, 'b) ease_route_t -> float 
  (* Sum of squares of all search radii *)

val eucl_length : dist_f:(Coord.coordf_t -> Coord.coordf_t -> float) ->
  (Coord.coordf_t, 'b) t -> float
  (* Compute Euclidean length of route given a distance function *)


val i2c : (Common.nodeid_t, 'b) t -> (Coord.coordf_t, 'b) t

val sprint : (Coord.coordf_t, 'b) t -> string  

