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

type basic_stats = 
    { pkts_RX : int;
      bits_RX : int;
      pkts_TX : int;
      bits_TX : int}

open Misc 

class type t  = 
object

  inherit Log.inheritable_loggable 
  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method xmit : L2pkt.t -> unit
  method bps : float
  method basic_stats : basic_stats
  method reset_stats : unit
end


class type ['stats] backend_t = 
object
    method xmit : L2pkt.t -> unit

    method private backend_reset_stats : unit
    method private backend_stats : 'stats
    method private backend_recv : L2pkt.t -> unit
    method private backend_xmit_complete : unit
    method private unicast_failure : L2pkt.t -> unit
end

type frontend_state = 
    Idle 
  | Rx   
  | Tx   

class type virtual ['stats] frontend_t = 
object 
  method private frontend_state : frontend_state
  method private frontend_reset_stats : unit
  method private frontend_stats : 'stats
  method private frontend_xmit : L2pkt.t -> unit
  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method bps : float
  method virtual private backend_recv : L2pkt.t -> unit
  method virtual private backend_xmit_complete : unit

  method basic_stats : basic_stats
end

class type ['stats] stats_t  = 
object
  inherit t
  method other_stats : 'stats
end

type mactype = Nullmac |  QueueNullmac | Contmac | Cheatmac | MACA_simple | MACA_contention | Tdackmac

let mac_ = ref Nullmac

let str2mac s = 
  match s with 
    | "null" |  "nullmac" -> Nullmac
    | "cheat" |  "cheatmac" -> Cheatmac
    | "contention" | "cont" | "contmac" -> Contmac
    | "maca_simple" -> MACA_simple
    | "MACA_contention" -> MACA_contention
    | "tdack" | "tdackmac" -> Tdackmac
    | _ -> raise (Failure "Invalid format for mac type")

let strset_mac s = 
  mac_ := str2mac s

let mac() = !mac_

