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



open Misc

class virtual ['stats] base ?(stack=0) ~bps owner = 
object(s)

  inherit Log.inheritable_loggable as log

  val mutable bTX = 0
  val mutable bRX = 0
  val myid = owner#id
  val owner:#Simplenode.simplenode = owner

  method basic_stats = {Mac.bits_RX = bTX; Mac.bits_TX = bRX}
  method private reset_stats = bTX <- 0; bRX <- 0
    

  method private set_objdescr ?owner string = 
    
    log#set_objdescr ?owner ((Misc.slashify string)^(Misc.i2s stack))

  method private xmitdelay l2pkt = 
    let bytes = (L2pkt.l2pkt_size ~l2pkt) in
    (i2f (bytes * 8)) /. bps

  method private send_up ~l2pkt = 
    owner#mac_recv_pkt ~stack l2pkt

  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method virtual xmit : l2pkt:L2pkt.t -> unit
  method virtual bps : float
  method virtual other_stats : 'stats

end
