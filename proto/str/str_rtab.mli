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

(* xxx todo: if nexthop only returns valid entries, how does it work when we
   are using an invalid entry 
   which entry does using_entry update ?*)



(** STR Routing tables. *)

open Common

type t
  (** The type of STR routing tables. *)

type metric_t = AODV | STR

val create : int -> t

val (>>>) : Str_pkt.triple_t -> Str_pkt.triple_t -> bool

(*
val age_dist : t -> nodeid_t -> Str_pkt.st_pair
  (** [age_dist rt dst] Returns the age and hopcount of the best entry (valid or
    not) for [dst] in [rt]. 
    @raise Not_found if [rtab] contains no routing entry for [dest]. *)

val valid_age_dist : t -> nodeid_t -> Str_pkt.st_pair 
  (** [valid_age_dist rt dst] Returns the age and hopcount of the best valid entry for
    [dst] in [rt]. 
    
    @raise Not_found if [rtab] contains no routing entry for [dest]. *)

val hopcount : t -> nodeid_t -> int 
  (** [hopcount rtab dest] returns the hop count of the best entry (valid or not) for
    [dst].
    @raise Not_found if [rtab] contains no routing entry for [dest] *)
  
val hopcount_opt : t -> nodeid_t -> int option
  (** Same as [hopcount] except that hopcount is returned as an option, and None
    is returned when no there is no entry for the destination. *)
  

*)


val cost : metric_t -> Str_pkt.triple_t -> float 
  (** Returns the binding metric_t cost of the age, distance pair. *)

val nexthop : t -> nodeid_t -> nodeid_t 
  (** [nexthop rtab dest] returns the nexthop of the best valid entry for [dest].
    @raise Not_found if [rtab] contains no valid entry for [dst]. *)

val nexthop_opt : t -> nodeid_t -> nodeid_t option
  (** Same as [nexthop] except that nexthop is returned as an option, and None
    is returned when no valid entry. *)

val invalidate_nexthop : t -> nodeid_t -> unit
  (** [invalidate rt nexthop] invalidates all valid entries [rt] which have
    [nexthop] as next hop. *) 


val best_valid_entry : t -> nodeid_t -> Str_pkt.triple_t * nodeid_t
val best_usable_invalid_entry : t -> metric_t -> nodeid_t -> Str_pkt.triple_t * nodeid_t

val better_valid_route : t -> ?offset:bool -> Str_pkt.str_hdr -> nodeid_t -> 
  (Str_pkt.triple_t * Common.nodeid_t) 
(** [better_valid_route rtab hdr dst]
  returns the best valid route in [rtab] which is 'better' than the one in
  the header, along with the nexthop. Returns a [Str_pkt.null_triple]
  if the routing table contains no better valid  routes. *)
  
val better_usable_invalid_route : t -> metric_t -> ?offset:bool -> Str_pkt.str_hdr -> nodeid_t -> 
  (Str_pkt.triple_t * Common.nodeid_t) 
  (** [better_invalid_route rtab hdr dst]
    returns the best usable invalid route in [rtab] which is 'better' than the one in
    the header, along with the nexthop. Returns a [Str_pkt.null_triple]
    if the routing table contains no better invalid routes. *)
  
val better_invalid_route : t -> metric_t -> ?offset:bool -> Str_pkt.str_hdr -> nodeid_t -> 
  Str_pkt.triple_t
  (** [better_invalid_route rtab hdr dst]
    returns the best invalid route in [rtab] which is 'better' than the one in
    the header, along with the nexthop. This may be an unusable invalid route,
    ie a route which has not been used within the previous
    {!Str_defaults.ACTIVE_ROUTE_TIMEOUT} seconds. Returns a
    [Str_pkt.null_triple] if the routing table contains no better invalid routes. *)
  
val using_entry : t -> nodeid_t -> Str_pkt.triple_t -> unit
  (** [using_valid_entry rtab dest] updates the last_used timestamp for the 
    entry [ent] to destination [dest] in [rtab] (ie, the entry that would be
    returned by [nexthop rtab dest]).
    This should be called each time we use a routing entry to forward a
    unicast packet. *)

val add_entry : t -> valid:bool -> dst:nodeid_t ->
  ent:Str_pkt.triple_t -> nh:nodeid_t -> bool
  

val clear_entry : t -> nodeid_t -> unit
  (** Set entry for dst back to 'empty' state (ie, state when a routing table
    is initially created) *)

val clear_all_entries : t -> unit
  (** Set all entries back to 'empty' state (ie, state when a routing table
    is initially created). *)

val sprint_entries : t -> nodeid_t -> string
  (** [sprint_entries rt dst] returns a text representation of the routing
    entries in [rt] for destination [dst]. *)

(*
val sprint_best_entry : t -> nodeid_t -> string
  (** [sprint_entries rt dst] returns a text representation of the best
    routing entry (the one with lowest cost) in [rt] for destination [dst]. *)

val sprint_best_valid_entry : t -> nodeid_t -> string
  (** [sprint_entries rt dst] returns a text representation of the best valid
    routing entry (the one with lowest cost) in [rt] for destination [dst]. *)



*)
