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

(** STR Routing tables. *)

open Common

type t
  (** The type of STR routing tables. *)


val create : int -> t

val repair_start : t -> nodeid_t -> unit
  (** [repair_start rtab dst] should be called each time a route repair is
    initiated for destination [dst]. *)

val repair_end  : t -> nodeid_t -> unit
  (** [repair_end rtab dst] marks in [rtab] that the repair for [dst] is not
    ongoing anymore. This should be called when a route request is abandoned.
    It is not necessary when a route request is succesfully completed. *)

val repairing : t -> nodeid_t -> bool
  (** [repairing rtab dst] returns [true] if we have a started route repair
    for [dst] and the route has not been revalidated. This includes the case
    where we have sent a route request for [dst] and then learned of a route
    to [dst] through some other mechanism (ie, because [dst] flooded some rreq
    itself), even when our original route request is ongoing. *)
  
val age_dist : t -> nodeid_t -> (Str_pkt.age_t * int) 
  (** [age_dist rt dst] Returns the age and hopcount of the best entry (valid or
    not) for [dst] in [rt]. 
    @raise Not_found if [rtab] contains no routing entry for [dest]. *)

val valid_age_dist : t -> nodeid_t -> (Str_pkt.age_t * int) 
  (** [valid_age_dist rt dst] Returns the age and hopcount of the best valid entry for
    [dst] in [rt]. 
    
    @raise Not_found if [rtab] contains no routing entry for [dest]. *)

val cost : (Str_pkt.age_t * int) -> float
  (** Returns the binding metric cost of the age, distance pair. *)

val nexthop : t -> nodeid_t -> nodeid_t 
  (** [nexthop rtab dest] returns the nexthop of the best valid entry for [dest].
    @raise Not_found if [rtab] contains no valid entry for [dst]. *)

val nexthop_opt : t -> nodeid_t -> nodeid_t option
  (** Same as [nexthop] except that nexthop is returned as an option, and None
    is returned when no valid entry. *)

val hopcount : t -> nodeid_t -> int 
  (** [hopcount rtab dest] returns the hop count of the best entry (valid or not) for
    [dst].
    @raise Not_found if [rtab] contains no routing entry for [dest] *)
  
val hopcount_opt : t -> nodeid_t -> int option
  (** Same as [hopcount] except that hopcount is returned as an option, and None
    is returned when no there is no entry for the destination. *)
  
val invalidate_nexthop : t -> nodeid_t -> unit
  (** [invalidate rt nexthop] invalidates all valid entries [rt] which have
    [nexthop] as next hop. *) 

val have_better_valid_route : t -> float -> nodeid_t -> bool
  (** [have_better_valid_route rtab cost dest] returns true if [rtab] contains a
    valid route to [dest] with cost lower than [cost]. *)

val have_better_route : t -> ?offset:int -> float -> nodeid_t -> bool
  (** [have_better_route rtab cost dest] returns true if [rtab] contains a
    route (valid or not) to [dest] with cost lower than [cost]. 
    offset represents the distance travelled by the RREQ containing the (age,
    dist) pair of the originator's route.
*)

val using_entry : t -> nodeid_t -> unit
  (** [using_entry rtab dest] updates the last_used timestamp for the best
    valid entry to destination [dest] in [rtab] (ie, the entry that would be
    returned by [nexthop rtab dest]).
    This should be called each time we use a routing entry to forward a
    unicast packet. *)

val add_entry : t -> dst:nodeid_t -> age:Str_pkt.age_t -> nh:nodeid_t -> hc:int ->
  bool
  
val clear_entry : t -> nodeid_t -> unit
  (** Set entry for dst back to 'empty' state (ie, state when a routing table
    is initially created) *)

val clear_all_entries : t -> unit
  (** Set all entries back to 'empty' state (ie, state when a routing table
    is initially created). *)

val sprint_entries : t -> nodeid_t -> string
  (** [sprint_entries rt dst] returns a text representation of the routing
    entries in [rt] for destination [dst]. *)

val sprint_best_entry : t -> nodeid_t -> string
  (** [sprint_entries rt dst] returns a text representation of the best
    routing entry (the one with lowest cost) in [rt] for destination [dst]. *)

val sprint_best_valid_entry : t -> nodeid_t -> string
  (** [sprint_entries rt dst] returns a text representation of the best valid
    routing entry (the one with lowest cost) in [rt] for destination [dst]. *)

