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




class virtual base ?(stack=0) owner =
object(s : #Rt_agent.t)
  inherit Log.inheritable_loggable as log

  val myid = owner#id
  val owner:#Simplenode.simplenode = owner

  method myid = myid

  method private set_objdescr ?owner string = 
    log#set_objdescr ?owner ((Misc.slashify string)^(Misc.i2s stack))

  method private mac_bcast_pkt l3pkt = owner#mac_bcast_pkt ~stack l3pkt

  method private mac_send_pkt l3pkt ~dstid = 
    owner#mac_send_pkt ~stack ~dst:dstid l3pkt

  method private cheat_send_pkt l3pkt dstid = 
    owner#cheat_send_pkt ~stack ~dst:dstid l3pkt 

  method virtual mac_recv_l3pkt : L3pkt.t -> unit
  method virtual mac_recv_l2pkt : L2pkt.t -> unit
  method virtual app_recv_l4pkt : L4pkt.t -> Common.nodeid_t -> unit

  method private bps = (owner#mac ~stack ())#bps

end
