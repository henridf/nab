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

(* $Header$ *)



(** Simplenode: Encapsulates most of the state and constituent objects
  (routing agent, mac layer, traffic source) that model a node.

  @author Henri Dubois-Ferriere. 
*)




exception Mac_Send_Failure
  (** Exception raised when trying to unicast a packet to a non-neighboring
    node. *)

type node_state_t = Coord.coordf_t
    (** this type is used for storing node state to file *)



(** Simplenode: Encapsulates most of the state and constituent objects
  (routing agent, mac layer, traffic source) that model a node.

  {b On multiple stacks:}

  Some methods and functions have an optional [?stack] argument, allowing to
  indicate which stack is being used. This optional argument
  defaults to 0; therefore one need never specify it if running only one
  stack (and can hence can safely ignore this passage).

  We explain the notion of multiple stacks below:

  It is possible in mws to simultaneously run multiple protocol stacks on each
  node. These are completely oblivious to, and independent of, each
  other. Running multiple stacks can be helpful for example when comparing
  different routing protocols, parameters, MAC layers, or combinations
  thereof: instead of scripting as many simulation runs as configurations, and
  running them sequentially, it is possible to do them all together. This can
  remove quite some scripting hassle, and allows to be confident that each
  protocol is being presented with {i exactly} the same conditions in terms of
  mobility, traffic, etc.

  Using multiple stacks together can also be faster, since all the
  computations related to node mobility, computing node neighbors, etc (which
  can be quite CPU-intensive)  are done once only, rather than repeating
  these computations in as many separate runs.

  Note that since a stack includes the MAC layer, and since contention happens in
  the MAC layer, multiple stacks will not interfere with each other in terms
  of contention/collisions.

 *)
class simplenode : Common.nodeid_t ->

object ('a)
  
  inherit Log.inheritable_loggable 

  method dump_state : node_state_t 
    
  method id : Common.nodeid_t

  (** Installing/removing components of a protocol stack. *)

  method install_mac : ?stack:int -> Mac.t -> unit
    (** Install a {!Mac.t} object in the given [stack] (stack 0 if not specified). *)

  method mac : ?stack:int -> unit -> Mac.t
    (** Returns the node's {!Mac.t} object from the given [stack] (stack 0 if not specified). *)

  method remove_mac : ?stack:int -> unit -> unit
    (** Remove {!Mac.t} from given [stack] (stack 0 if not specified). *)

  method install_rt_agent : ?stack:int -> Rt_agent.t -> unit
    (** Install a {!Rt_agent.t} object in the given [stack] (stack 0 if not specified). *)

  method agent : ?stack:int -> unit -> Rt_agent.t
    (** Returns the node's routing agent from the given [stack] (stack
      0 if not specified). Note that the routing agent is coerced to a
      {!Rt_agent.t}, so any additional methods than those in the {!Rt_agent.t}
      type are hidden. 
    *)
    
  method remove_rt_agent : ?stack:int -> unit -> unit
    (** Remove {!Rt_agent.t} from given [stack] (stack 0 if not specified). *)

  method remove_stack : ?stack:int -> unit -> unit
    (** Remove entire protocol stack [stack] (stack 0 if not specified). *)


  (** Sending/receiving packets. *)

  method set_trafficsource :  gen:Trafficgen.t ->  dst:Common.nodeid_t -> unit
    (** Installs a traffic source and uses it to generate application packets
      to node dst. 

      Multiple trafficsources to multiple destinations can be installed (XXX not
      tested though)
    *)
   
  method mac_recv_pkt : ?stack:int -> L2pkt.t -> unit

  method mac_send_pkt : 
    ?stack:int -> 
    dst:Common.nodeid_t -> 
    L3pkt.t ->  unit
    (** Sent packet to neighbor mac_dst. Raises {!Simplenode.Mac_Send_Failure} if mac_dst is not a neighbor *)

  method cheat_send_pkt : ?stack:int -> dst:Common.nodeid_t -> L3pkt.t -> unit
    (** Same as above except will accept sending pkt to any destination, even
      if that destination is not a neighbor within range (hence the "cheating"). 
      This only works if the MAC layer is a {!Mac_cheat.cheatmac} - otherwise
      a packet to a node which is not in range is silently dropped.
    *)
    
  method mac_bcast_pkt : ?stack:int -> L3pkt.t -> unit
    (** Broadcast packet to all neighbors. *)
    
  method originate_app_pkt : dst:Common.nodeid_t -> unit
    (** originates a packet from an application on this node to dst:
      create the packet and shove it down the app_send_pkt_hooks *)
    
  (** Inserting hooks. *)
    
  method add_pktin_mhook : ?stack:int -> (L2pkt.t -> 'a -> unit) -> unit
    (** Any monitoring application can register here to see all packets entering
      the node.
      If multiple apps, order in which called is unspecified.*)
    
  method add_pktout_mhook : ?stack:int -> (L2pkt.t -> 'a -> unit) -> unit
    (** Any monitoring application can register here to see all packets leaving
      the node.
      If multiple apps, order in which called is unspecified.*)
    
  method clear_pkt_mhooks : ?stack:int -> unit -> unit 
    (** clears pktin and pktout mhooks *)


end
  
val max_nstacks : int

(**/**)  
    
    
    
