(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)



(** Base class for routing agents. 
  This class essentially serves to hide multistack details (see {!Simplenode.simplenode}
  for an explanation of multiple stacks) from the derived
  subclasses, to avoid having to keep track of which stack an agent is in
  (since in most cases nodes are run with only one stack).

  The class also specifies virtual methods which must be implemented in order
  to conform to the {!Rt_agent.t} interface.

  When implementing a new routing agent, it is recommended to inherit from
  this class.

  @author Henri Dubois-Ferriere.
*)

class virtual base : ?stack:int -> #Simplenode.simplenode ->
  (** [stack] serves to distinguish when multiple stacks are being used. 
    The notion of multiple stacks is explained in {!Simplenode.simplenode}.*)
object

  inherit Log.inheritable_loggable

  val myid : Common.nodeid_t
  val owner:#Simplenode.simplenode

  method myid : Common.nodeid_t

  method virtual mac_recv_l3pkt : L3pkt.t -> unit
    (** See {!Rt_agent.t} *)
  method virtual mac_recv_l2pkt : L2pkt.t -> unit
    (** See {!Rt_agent.t} *)
  method virtual app_recv_l4pkt : L4pkt.t -> Common.nodeid_t -> unit
    (** See {!Rt_agent.t} *)

  method private mac_bcast_pkt : L3pkt.t -> unit
    (** Call this method from the derived routing agent to send a packet as a
      Mac-layer broadcast. This is a wrapper around [method mac_bcast_pkt] in
      {!Simplenode.simplenode}. *)

  method private mac_send_pkt : L3pkt.t -> dstid:Common.nodeid_t -> unit
    (** Call this method from the derived routing agent to send a packet as a
      Mac-layer unicast. This is a wrapper around [method mac_send_pkt] in
      {!Simplenode.simplenode}. *)

  method private cheat_send_pkt : L3pkt.t -> Common.nodeid_t -> unit
    (** Call this method from the derived routing agent to send a packet
      directly to any node. This is a wrapper around [method cheat_send_pkt] in
      {!Simplenode.simplenode}. 

      This only works if the MAC layer is a {!Mac_cheat.cheatmac} - otherwise
      a packet to a node which is not in range is silently dropped.
    *)

  method private bps : float
    (** Returns the speed of the underlying MAC layer for the node this agent
      is running on. *)

end
