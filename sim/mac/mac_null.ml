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
  Array.init Simplenode.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i

class nullmac ?(stack=0) bps theowner  = 
object(s)

  inherit [unit] Mac_base.base ~stack ~bps theowner as super

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/nullmac";
    Hashtbl.replace macs_array_.(stack) theowner#id (s :> nullmac);
  )

  method bps = bps

  method reset_stats = super#reset_stats

  method recv ?snr  ~l2pkt () = (

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
	  s#accept_ l2pkt;

      | d when (d = myid) ->  s#log_debug  (lazy
	  (sprintf "Start RX, l2src %d, l2dst %d" (l2src l2pkt) d));
	  s#accept_ l2pkt;

      | d -> s#log_debug  (lazy
	  (sprintf "Start RX, l2src %d, l2dst %d (not for us)" (l2src l2pkt) d));
    end
  )

  (* Called when we receive a packet which we keep 
     (either unicast to us, or broadcast). *)
  method private accept_ l2pkt = (
    
    (* Compute delay to receive whole packet. Remember that recv() below is
       called at the very beginning of the packet reception, so we shouldn't
       hand this packet to upper layers until the whole thing has arrived. *)
    let t = Time.get_time() +. 
      super#xmitdelay l2pkt in
    

    (* After the above delay, we will call send_up on our super class, which deals
       with pushing the packet into our node's protocol stack. *)
    let recv_event() =  super#send_up ~l2pkt in
    
    (Sched.s())#sched_at ~f:recv_event ~t:(Scheduler.Time t)	  
  )

  method xmit ~l2pkt = (

    let l2dst = (L2pkt.l2dst l2pkt) in
    if l2dst = L2pkt.l2_bcast_addr ||
      ((World.w())#are_neighbors myid l2dst)
    then (
      s#log_debug (lazy "TX packet ");
      SimpleEther.emit ~stack ~nid:myid l2pkt;
      pktsTX <- pktsTX + 1;
      bitsTX <- bitsTX + (L2pkt.l2pkt_size ~l2pkt)
    ) else s#unicast_failure l2pkt

  )

  method other_stats = ()

end
      
    
    
