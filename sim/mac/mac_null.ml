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

(* $Header *)







(** 
  Null MAC Layer: A simple example of a class implementing the {!Mac.t}
  interface.
  
  "Null" meaning that there are no collisions, no losses, only transmission
  delay is applied.
*)


open Ether
open L2pkt
open Printf 

class nullmac ?(stack=0) bps theowner  = 
object(s)

  inherit [unit] Mac_base.base ~stack ~bps theowner as super

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/nullmac";
  )

  method bps = bps

  method reset_stats = super#reset_stats

  method recv ?snr ~l2pkt () = (

    let dst = l2dst ~pkt:l2pkt in

    (* Count as incoming bits even when the packet wasn't for us - which is fair,
       this MAC is so basic it would be odd if it did fancy things like selective stop
       listening after reading hdr not for us *)
    bRX <- bRX + (L2pkt.l2pkt_size l2pkt);

    (* Throw away unicast packet if not for us, keep it otherwise *)
    begin match dst with
      | L2_BCAST ->  s#log_debug (lazy
	  (sprintf "Start RX, l2src %d, l2dst broadcast" (l2src ~pkt:l2pkt)));
	  s#accept_ l2pkt;

      | L2_DST d when (d = myid) ->  s#log_debug  (lazy
	  (sprintf "Start RX, l2src %d, l2dst %d" (l2src ~pkt:l2pkt) d));
	  s#accept_ l2pkt;

      | L2_DST d ->  s#log_debug  (lazy
	  (sprintf "Start RX, l2src %d, l2dst %d (not for us)" (l2src ~pkt:l2pkt) d));
    end

  )

  (* Called when we receive a packet which we keep (either unicast to us, or
     broadcast). *)
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
    s#log_debug (lazy "TX packet ");
    SimpleEther.emit ~stack ~nid:myid l2pkt;
    bTX <- bTX + (L2pkt.l2pkt_size l2pkt)
  )

  method other_stats = ()

end
      
    
    
