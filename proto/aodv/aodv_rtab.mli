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

(** AODV Routing tables. *)


type t
  (** The type of AODV routing tables. *)


val create : int -> t

val repair_start : t -> Common.nodeid_t -> unit
  (** [repair_start rtab dst] should be called each time a route repair is
    initiated for destination [dst]. *)

val repair_end  : t -> Common.nodeid_t -> unit
  (** [repair_end rtab dst] marks in [rtab] that the repair for [dst] is not
    ongoing anymore. This should be called when a route request is abandoned.
    It is not necessary when a route request is is succesfully completed. *)

val repairing : t -> Common.nodeid_t -> bool
  (** [repairing rtab dst] returns [true] if we have a started route repair
    for [dst] and the route has not been revalidated. This includes the case
    where we have sent a route request for [dst] and then learned of a route
    to [dst] through some other mechanism (ie, because [dst] flooded some rreq
    itself), even when our original route request is ongoing. *)
  
val set_lifetime : t -> Common.nodeid_t -> Time.t -> unit
  (** [set_lifetime rtab dst t] sets the lifetime for [dst] to be [t].
    [t] is a {e relative} time.
    Does nothing if [rtab] contains no entry for this destination. *)

val lifetime : t -> Common.nodeid_t -> Time.t 
  (** [lifetime rtab dst] returns the remaining lifetime of the entry for [dst]
     if the entry exists and is valid, 0.0 otherwise. *)

val seqno : t -> Common.nodeid_t -> Aodv_pkt.seqno_t option
  (** [seqno rt dst] Returns the sequence number for dst in rt. Returns None
    if no entry. *)

val incr_seqno : t -> Common.nodeid_t -> unit
  (** Does nothing if [rtab] contains no entry for this destination, or if
    [rtab] contains an entry but without a valid sequence number. *)

val set_seqno : t -> Common.nodeid_t -> Aodv_pkt.seqno_t -> unit
  (** Does nothing if [rtab] contains no entry for this destination. *)  

val nexthop : t -> Common.nodeid_t -> Common.nodeid_t 
  (** [nexthop rtab dest] returns the nexthop entry for [dest] if contains a
     valid entry.
     @raise Not_found if [rtab] contains no entry for [dst] or if entry is
     invalid. *)

val nexthop_invalid : t -> Common.nodeid_t -> Common.nodeid_t
  (** [nexthop rtab dest] returns the nexthop entry for [dest] if contains an
    entry (valid or not).
     @raise Not_found if [rtab] contains no entry for [dst].*)

val nexthop_opt : t -> Common.nodeid_t -> Common.nodeid_t option
  (** [nexthop_opt rtab dest] returns None if [rtab] contains no routing
     entry for [dest], or if our routing entry is invalid. If [rtab] contains
     a valid entry [nid], the function call returns [Some nid]. *)
  
val hopcount : t -> Common.nodeid_t -> int 
  (** [hopcount rtab dest] returns the hop count in the routing entry for
    [dst].
    @raise Not_found if [rtab] contains no routing entry for [dest] *)
  
val hopcount_opt : t -> Common.nodeid_t -> int option
  (** [hopcount rtab dest] returns 
    - [Some hc] where [hc] is the hop count in the routing entry for [dst], if
    there is an entry
    - [None] if there is no routing entry for [dst].
  *)
  
val invalidate : t -> Common.nodeid_t -> unit
  (** [invalidate rt dst] invalidates the entry for [dst] in [rt], ie unsets
    the 'valid' flag and clears the precursor list. *) 

val valid : t -> Common.nodeid_t -> bool
  (** [valid rtab dest] returns true if [rtab] contains a valid route to
    [dest]. *)

val add_entry_rreq : t -> Aodv_pkt.rreq -> Common.nodeid_t -> unit 
  (** [add_entry_rreq rtab rreq nexthop] adds or updates a routing entry to the
    originator of a route request (rfc 6.5), using the fields contained in the
    [rreq], with next hop [nexthop]. 

    If there is an existing entry for the rreq originator: 
    - The sequence number is overridden only if the proposed sequence number
    is fresher.
    - Next hop and hop count are always overriden (even if this results in a
    longer route).
     
    Lifetime of the route entry is updated as per rfc 6.5.

  *)
  

val add_entry_neighbor : t -> Common.nodeid_t -> unit 
  (** [add_entry_neighbor t dst seqno ] adds or updates a one-hop routing entry for
    destination [dst], setting nexthop to [dst] and hopcount to 1. The sequence
    number is set to to [None] (ie invalid sequence number, see rfc 6.2) if
    there was no entry, or is unchanged if there was a prior entry.
    Lifetime is updated to ACTIVE_ROUTE_TIMEOUT.
    Repairing status is set to [false].
 *)

val add_entry_rrep : t -> Aodv_pkt.rrep -> Common.nodeid_t -> bool
  (** [add_entry_rrep t rrep nexthop] adds or updates a routing entry to the
    destination of a route reply, using the fields contained in the packet
    [rrep], and next hop [nexthop]. The hopcount field in the packet is
    assumed {e not to have been incremented after reading off the rrep} (ie
    the value inserted into the routing table is incremented first).

    Route entry and lifetime are created or updated as per the description in rfc 6.7.
    Returns [true] if the route entry was updated, [false] otherwise.
    Repairing status is set to [false] if the route is updated.
*)

val dests_thru_hop : t -> Common.nodeid_t -> Common.nodeid_t list
  (** [dests_thru_hop rtab nexthop] returns the list of all dests for which
    whom [rtab] contains a valid entry, with next hop equal to [nexthop]. 
    The list may include [nexthop] (if [rtab]  has a valid entry to [nexthop]).
*)

val precursors : t ->  Common.nodeid_t -> Common.nodeid_t list
  (** [precursors rtab dest] returns the precursors of [dest] (empty list if no
     routing entry for dest). *)

val have_many_precursors : t ->  Common.nodeid_t list -> bool
  (** [has_many_precursors rtab destlist] returns true if, grouped together,
     the nodes in [destlist] have a number of (distinct) precursors greater
     than one. *)

val has_precursors : t ->  Common.nodeid_t -> bool
  (** [has_precursors rtab dest] returns true if there are precursors for node
     [dest] in the routing table, false otherwise. *)

val add_precursor : t -> dst:Common.nodeid_t -> pre:Common.nodeid_t -> unit
  (** [add_precursor rtab ~dst ~pre] adds [pre] to the precursor list of [dst] *)


val clear_entry : t -> Common.nodeid_t -> unit
  (** Set entry for dst back to 'empty' state (ie, state when a routing table
    is initially created *)

val clear_all_entries : t -> unit
  (** Set all entries back to 'empty' state (ie, state when a routing table
    is initially created). *)

val have_active_route : t -> bool
  (** Returns [true] if there is any valid route in the routing table, ie if
    we are part of an active route. *)
