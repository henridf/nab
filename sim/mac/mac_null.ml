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


open Ether
open L2pkt
open Printf 

let macs_array_ = 
  Array.init Node.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i

class nullmac ?(stack=0) ~bps theowner  = 
  let myid = theowner#id in
object(s)

  inherit [unit] Mac_base.base ~stack ~bps theowner as super

  val mutable bitsTX = 0 
  val mutable bitsRX = 0
  val mutable pktsRX = 0 
  val mutable pktsTX = 0

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/nullmac";
    Hashtbl.replace macs_array_.(stack) theowner#id (s :> nullmac);
  )

  method bps = bps

  method reset_stats = super#reset_stats

  method recv ?snr ~l2pkt () = (

    let dst = l2dst l2pkt in

    (* Count as incoming bits even when the packet wasn't for us - which is fair,
       this MAC is so basic it would be odd if it did fancy things like selective stop
       listening after reading hdr not for us *)
    pktsRX <- pktsRX + 1;
    bitsRX <- bitsRX + (L2pkt.l2pkt_size ~l2pkt);

    (* Throw away unicast packet if not for us, keep it otherwise *)
    begin match dst with
      | d when (d = l2_bcast_addr) ->  s#log_debug (lazy
	  (sprintf "Start RX, l2src %d, l2dst broadcast" (l2src l2pkt)));
      | d when (d = myid) ->  s#log_debug  (lazy
	  (sprintf "Start RX, l2src %d, l2dst %d" (l2src l2pkt) d));
      | d -> assert(false); 
    end;

    super#send_up l2pkt;
  )

  method xmit l2pkt = 
    let l2dst = L2pkt.l2dst l2pkt in
    let neighbors = (World.w())#neighbors myid in
    if l2dst = L2pkt.l2_bcast_addr ||
      (List.mem l2dst neighbors) then (

	s#log_debug (lazy (sprintf "TX packet to %d" l2dst));
	pktsTX <- pktsTX + 1;
      bitsTX <- bitsTX + (L2pkt.l2pkt_size ~l2pkt);

    let t = Mac_base.xmit_time bps l2pkt 
    and mypos = ((World.w())#nodepos myid) in

    List.iter (fun id -> 
      if id <> myid && (l2dst = L2pkt.l2_bcast_addr || l2dst = id) then (
	let n = (Nodes.node(id)) in
	let recvtime = t +. (propdelay mypos ((World.w())#nodepos myid)) in
	let recv_event() = 
	  (n#mac ~stack ())#recv ~l2pkt () in
	(Sched.s())#sched_in ~f:recv_event ~t
      )
    ) neighbors
    ) else s#unicast_failure l2pkt


  method other_stats = ()

end
      
    
    
