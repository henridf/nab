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



(** Base class for routing agents. 
  This class essentially serves to hide multistack details (see {!Node.node}
  for an explanation of multiple stacks) from the derived
  subclasses, to avoid having to keep track of which stack an agent is in
  (since in most cases nodes are run with only one stack).

  The class also specifies virtual methods which must be implemented in order
  to conform to the {!Rt_agent.t} interface.

  When implementing a new routing agent, it is recommended to inherit from
  this class.

  @author Henri Dubois-Ferriere.
*)

class virtual ['a] base : ?stack:int -> #Node.node ->
  (** [stack] serves to distinguish when multiple stacks are being used. 
    The notion of multiple stacks is explained in {!Node.node}.*)
object

  inherit Log.inheritable_loggable

  val myid : Common.nodeid_t
  val owner:#Node.node

  method myid : Common.nodeid_t

  method virtual recv_pkt_mac : 
    l2src:Common.nodeid_t -> l2dst:Common.nodeid_t -> L3pkt.t -> unit
    (** See {!Rt_agent.t} *)

  method virtual recv_pkt_app : L4pkt.t -> Common.nodeid_t -> unit
    (** See {!Rt_agent.t} *)
  method mac_callback : L3pkt.t -> Common.nodeid_t -> unit
    (** See {!Rt_agent.t} *)

  method virtual stats : 'a

  method private mac_bcast_pkt : L3pkt.t -> unit
    (** Call this method from the derived routing agent to send a packet as a
      Mac-layer broadcast. This is a wrapper around [method mac_bcast_pkt] in
      {!Node.node}. *)

  method private mac_send_pkt : L3pkt.t -> Common.nodeid_t -> unit
    (** Call this method from the derived routing agent to send a packet as a
      Mac-layer unicast. This is a wrapper around [method mac_send_pkt] in
      {!Node.node}. *)

  method private bps : float
    (** Returns the speed of the underlying MAC layer for the node this agent
      is running on. *)


end

class virtual ['a, 'b] base_persist : ?stack:int -> #Node.node -> 
object
  inherit ['a] base 
  method virtual dump_state : unit -> 'b
  method virtual read_state : 'b -> unit
end
