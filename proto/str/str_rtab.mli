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
    It is not necessary when a route request is is succesfully completed. *)

val repairing : t -> nodeid_t -> bool
  (** [repairing rtab dst] returns [true] if we have a started route repair
    for [dst] and the route has not been revalidated. This includes the case
    where we have sent a route request for [dst] and then learned of a route
    to [dst] through some other mechanism (ie, because [dst] flooded some rreq
    itself), even when our original route request is ongoing. *)
  
val seqno : t -> nodeid_t -> Str_pkt.seqno_t option
  (** [seqno rt dst] Returns the sequence number for dst in rt. Returns None
    if no entry. *)

val set_seqno : t -> nodeid_t -> Str_pkt.seqno_t -> unit
  (** Does nothing if [rtab] contains no entry for this destination. *)  

val nexthop : t -> nodeid_t -> nodeid_t 
  (** [nexthop rtab dest] returns the nexthop entry for [dest] if contains a
     valid entry.
     @raise Not_found if [rtab] contains no entry for [dst] or if entry is
     invalid. *)

val nexthop_invalid : t -> nodeid_t -> nodeid_t
  (** [nexthop rtab dest] returns the nexthop entry for [dest] if contains an
    entry (valid or not).
     @raise Not_found if [rtab] contains no entry for [dst].*)

val nexthop_maybe : t -> nodeid_t -> nodeid_t option
  (** [nexthop_maybe rtab dest] returns None if [rtab] contains no routing
     entry for [dest], or if our routing entry is invalid. If [rtab] contains
     a valid entry [nid], the function call returns [Some nid]. *)
  
val hopcount : t -> nodeid_t -> int 
  (** [hopcount rtab dest] returns the hop count in the routing entry for
    [dst].
    @raise Not_found if [rtab] contains no routing entry for [dest] *)
  
val hopcount_maybe : t -> nodeid_t -> int option
  (** [hopcount rtab dest] returns 
    - [Some hc] where [hc] is the hop count in the routing entry for [dst], if
    there is an entry
    - [None] if there is no routing entry for [dst].
  *)
  
val invalidate : t -> nodeid_t -> unit
  (** [invalidate rt dst] invalidates the entry for [dst] in [rt], ie unsets
    the 'valid' flag and clears the precursor list. *) 

val valid : t -> nodeid_t -> bool
  (** [valid rtab dest] returns true if [rtab] contains a valid route to
    [dest]. *)

val add_entry : t -> dst:nodeid_t -> seqno:Str_pkt.seqno_t -> nh:nodeid_t
  -> hc:int -> bool
  
val ( >>> ) : Str_pkt.seqno_t * int -> Str_pkt.seqno_t * int -> bool
  (* Space-time ordering function *)


val dests_thru_hop : t -> nodeid_t -> nodeid_t list
  (** [dests_thru_hop rtab nexthop] returns the list of all dests for which
    whom [rtab] contains a valid entry, with next hop equal to [nexthop]. *)

val clear_entry : t -> nodeid_t -> unit
  (** Set entry for dst back to 'empty' state (ie, state when a routing table
    is initially created) *)

val clear_all_entries : t -> unit
  (** Set all entries back to 'empty' state (ie, state when a routing table
    is initially created). *)

val have_active_route : t -> bool
  (** Returns [true] if there is any valid route in the routing table, ie if
    we are part of an active route. *)

val have_entry : t -> nodeid_t -> bool
