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

(* $Id$ *)



(** Routing Agent interface.
 
  @author Henri Dubois-Ferriere.
 *)

(** The interface which a routing agent must implement.
  
  Note for those implementing a routing agent: it is simplest to inherit
  from {!Rt_agent_base.base}, for the reasons described therein.
*)
class type t = 
object

  method myid : Common.nodeid_t
    (** Returns the id of the node to which this agent is attached. *)

  method mac_recv_l3pkt : L3pkt.t -> unit
    (** This method will be called whenever a packet arrives from the MAC layer.*)

  method mac_recv_l2pkt : L2pkt.t -> unit
    (** Same as [#mac_recv_l3pkt], except that the packet comes
      with L2 hdr still on, for those agents which might need it. *)

  method app_recv_l4pkt : L4pkt.t -> Common.nodeid_t -> unit
    (** This method is called with outbound packets generated by upper-layer
      (application) agents or traffic sources.*)

end
