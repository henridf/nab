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




class virtual ['a] base ?(stack=0) owner =
object(s : #Rt_agent.t)
  inherit Log.inheritable_loggable as log

  val myid = owner#id
  val owner:#Node.node = owner

  method myid = myid

  method private set_objdescr ?owner string = 
    log#set_objdescr ?owner ((Misc.slashify string)^(Misc.i2s stack))

  method private mac_bcast_pkt l3pkt = owner#mac_bcast_pkt ~stack l3pkt

  method private mac_send_pkt l3pkt dst = 
    owner#mac_send_pkt ~stack dst l3pkt

  method mac_callback l3pkt nexthop = ()

  method virtual recv_pkt_mac : 
    l2src:Common.nodeid_t -> l2dst:Common.nodeid_t -> L3pkt.t -> unit

  method virtual recv_pkt_app : L4pkt.t -> Common.nodeid_t -> unit
  method virtual stats : 'a

  method private bps = (owner#mac ~stack ())#bps


end

class virtual ['a, 'b] base_persist  ?(stack=0) owner =
object
  inherit ['a] base ~stack owner
  method virtual dump_state : unit -> 'b
  method virtual read_state : 'b -> unit
end
