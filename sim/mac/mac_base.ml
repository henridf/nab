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

let macs_array_ = 
  Array.init Simplenode.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i



open Misc

class virtual ['stats] base ?(stack=0) ~bps owner = 
object(s)

  inherit Log.inheritable_loggable as log

  val mutable bitsTX = 0
  val mutable bitsRX = 0
  val mutable pktsTX = 0
  val mutable pktsRX = 0
  val myid = owner#id
  val owner:#Simplenode.simplenode = owner

  initializer (
    Hashtbl.replace macs_array_.(stack) owner#id (s :> Mac.t);
  )


  method basic_stats = {
    Mac.bits_RX = bitsRX; 
    Mac.bits_TX = bitsTX;
    Mac.pkts_RX = pktsRX; 
    Mac.pkts_TX = pktsTX
  }
  method reset_stats = 
    bitsTX <- 0; 
    bitsRX <- 0;
    pktsTX <- 0; 
    pktsRX <- 0
    

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

let string_of_bstats s = 
  (Printf.sprintf "RX: %d pkts (%d bits). TX: %d pkts (%d bits)" 
    s.Mac.pkts_RX
    s.Mac.bits_RX 
    s.Mac.pkts_TX
    s.Mac.bits_TX
  )
let string_of_bstats_pkts s = 
  (Printf.sprintf "RX: %d pkts. TX: %d pkts" 
    s.Mac.pkts_RX
    s.Mac.pkts_TX
  )

let string_of_bstats_bits s = 
  (Printf.sprintf "RX: %d bits. TX: %d bits" 
    s.Mac.bits_RX
    s.Mac.bits_TX
  )

let add_bstats s1 s2 = 
  {
    Mac.bits_RX = s1.Mac.bits_RX + s2.Mac.bits_RX; 
    Mac.bits_TX = s1.Mac.bits_TX + s2.Mac.bits_TX; 
    Mac.pkts_RX = s1.Mac.pkts_RX + s2.Mac.pkts_RX; 
    Mac.pkts_TX = s1.Mac.pkts_TX + s2.Mac.pkts_TX
  }
  


let zero_bstats () = 
  {
    Mac.bits_RX = 0;
    Mac.bits_TX = 0;
    Mac.pkts_RX = 0;
    Mac.pkts_TX = 0
  }
  
