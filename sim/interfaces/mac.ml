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


