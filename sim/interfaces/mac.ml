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


type basic_stats = 
    { bits_RX : int;
      bits_TX : int}

open Misc 

class type  t  = 
object

  inherit Log.inheritable_loggable 
  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method xmit : l2pkt:L2pkt.t -> unit
  method bps : float
  method basic_stats : basic_stats
  method reset_stats : unit
end
    
type mactype = Nullmac | Contmac | Cheatmac

let mac_ = ref Nullmac

let str2mac s = 
  match s with 
    | "null" |  "nullmac" -> Nullmac
    | "cheat" |  "cheatmac" -> Cheatmac
    | "contention" | "cont" | "contmac" -> Contmac
    | _ -> raise (Failure "Invalid format for mac type")

let strset_mac s = 
  mac_ := str2mac s

let mac() = !mac_


