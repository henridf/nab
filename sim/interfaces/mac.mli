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







(** 
  MAC layer interface and helper functions.

  @author Henri Dubois-Ferriere.
*)


(**  The interface which a MAC layer must implement.

  Note for those implementing a MAC layer: it is simplest to inherit
  from {!Mac_base.base}, for the reasons described therein.
  For a simple example of a MAC class, see {!Mac_null.nullmac}.
*)
class type t  = 
object

  inherit Log.inheritable_loggable 

  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
    (** [recv] is called when a packet arrives at a node.

      More precisely, [recv] is called when the {i first bit} of a packet
      arrives at the node. So, a realistic MAC implementation should wait an
      appropriate duration (depending on packet size and transmission rate of
      this MAC). A MAC that detects collisions would also keep track of the
      fact that it is receiving for this duration, in order to know that there
      is a collision if another packet is received in this interval.
    *)
      
  method xmit : l2pkt:L2pkt.t -> unit

  method bps : float
end
    
(** The types of MAC that are available. *)
type mactype = 
  | Nullmac  (** See {!Mac_null.nullmac} *)
  | Contmac  (** See {!Mac_contention.contentionmac} *)
  | Cheatmac (** See {!Mac_cheat.cheatmac} *)

val strset_mac : string -> unit
  (** Set the default mac via a string (for example provided as cmdline argument). *)

val mac : unit -> mactype
  (** Returns the mac type employed. *)


