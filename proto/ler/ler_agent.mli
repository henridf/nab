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


(** Simple EASE, GREASE, and FRESH Routing Agents.
 
  This is a simple implementation of last encounter routing protocols which
  takes a few global shortcuts, as opposed to a purely distributed
  implementation. This allows in particular to avoid implementing real
  packet-level floods for anchor searches, in order to have extreme
  scalability to tens of thousands of nodes.

  These shortcuts are:

  - Geographical Routing: When we are at position X, and have a packet
  addressed to, position Y, the next hop is chosen as the next closest node to
  y (after ourselves). This simple algorithm is guaranteed to arrive at the
  closest node to point Y without getting into dead-ends, etc.

  - Anchor Search: This is found by using {!Worldt.lazy_world_t.find_closest}, ie by
  searching globally (as opposed to flooding a real search packet with an
  expanding ring search, etc).

  - Neighbor Notification: We use {!Worldt.greedy_world_t.add_new_ngbr_hook}
  to be instantaneously notified each time a node comes into range (as opposed
  to sending and listening for periodic hello packets).

  @author Henri Dubois-Ferriere.
 *)


type ler_proto_t = EASE | GREASE | FRESH

type persist_t = 
    {le_state : Le_tab.le_tab_state_t;
    proto:ler_proto_t}


(** Pass [true] for [grease] argument to constructor to get a GREASE agent,
  [false] to get EASE. *)
class ler_agent : ?stack:int -> proto:ler_proto_t -> #Node.node -> 
object 

  inherit Rt_agent.t
    
  method le_tab : Le_tab.le_tab
  method stats : unit
  method read_state : persist_t -> unit
  method dump_state : unit -> persist_t
end




val ntargets : int Param.t
  (** The number of nodes that can potentially be routed to as destinations.
    In small simulations, this should be equal to the number of nodes. 
    For large simulations, some parts of nab may be more efficient when this
    is kept small. For example, in EASE routing, the size of the
    Last-Encounter table depends on the number of targets value. *)
  
val proportion_met_nodes : ?stack:int -> unit -> float
  (** Computes the encounter ratio, ie the proportion of source-dst pairs that
    have last-encounter entries for each other.
    This takes into account the value of [ntargets].*)



module Persist : Persist.t 
