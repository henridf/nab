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







(** EASE and GREASE Routing Agents.
 
  This is a simple implementation of [GR]EASE which takes some global shortcuts,
  ie it "cheats" compared to a purely distributed implementation.

  These shortcuts are:

  - Geographical Routing: When we are at position X, and have a packet
  addressed to, position Y, the next hop is chosen as the next closest node to
  y (after ourselves). This simple algorithm is guaranteed we will arrive at the
  closest node to point Y without getting into dead-ends, etc.

  - Anchor Search: This is found by using {!Worldt.lazy_world_t.find_closest}, ie by
  searching globally (as opposed to flooding a real search packet with an
  expanding ring search, etc).

  - Neighbor Notification: We use {!Worldt.greedy_world_t.add_new_ngbr_hook}
  to be instantaneously notified each time a node comes into range (as opposed
  to sending and listening for periodic hello packets).

  @author Henri Dubois-Ferriere.
 *)


(** Pass [true] for [grease] argument to constructor to get a GREASE agent,
  [false] to get EASE. *)
class ease_agent : ?stack:int -> grease:bool -> #Simplenode.simplenode -> 
object 
  inherit Log.inheritable_loggable
  inherit Rt_agent.t

  method le_tab : Le_tab.le_tab
  method set_le_tab : Le_tab.le_tab -> unit
end


val proportion_met_nodes : ?stack:int -> unit -> float


