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
  Array.init Node.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i

let xmit_time bps l2pkt = 
    let bytes = (L2pkt.l2pkt_size ~l2pkt) in
    (Misc.i2f (bytes * 8)) /. bps

open Misc
open Ether

class virtual ['stats] base ?(stack=0) ~(bps:float) (owner:#Node.node) = 
object(s)

  inherit Log.inheritable_loggable as log

  val mutable bitsTX = 0
  val mutable bitsRX = 0
  val mutable pktsTX = 0
  val mutable pktsRX = 0

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
    
  method private send_up l2pkt = 
    owner#mac_recv_pkt ~stack l2pkt

  method private unicast_failure l2pkt = 
    owner#mac_send_failure ~stack l2pkt

  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method virtual xmit : L2pkt.t -> unit
  method virtual other_stats : 'stats
  method bps = bps
end

class virtual ['stats] backend ?(stack=0) ~(bps:float) (owner:#Node.node) = 
object(s)

  inherit Log.virtual_loggable

  method virtual xmit : L2pkt.t -> unit

  method private send_up l2pkt = 
    owner#mac_recv_pkt ~stack l2pkt

  method private unicast_failure l2pkt = 
    owner#mac_send_failure ~stack l2pkt

  method virtual private backend_reset_stats : unit
  method virtual private backend_stats : 'stats
  method virtual private backend_xmit_complete : unit

end

class virtual null_backend ?(stack=0)  ~(bps:float)
  (owner:#Node.node) =
  let myid = owner#id in
object(s)
  inherit [unit] backend ~stack ~bps owner as super
  method private backend_reset_stats  = ()
  method private backend_stats = ()
  method private backend_recv l2pkt = 
    let dst = L2pkt.l2dst l2pkt in
    
    (* Throw away unicast packet if not for us, keep it otherwise *)
    begin match dst with
      | d when (d = L2pkt.l2_bcast_addr) ->  s#log_debug (lazy
	  (Printf.sprintf "Start RX, l2src %d, l2dst broadcast" 
	    (L2pkt.l2src l2pkt)));
	  super#send_up l2pkt;
	  
      | d when (d = myid) ->  s#log_debug  (lazy
	  (Printf.sprintf "Start RX, l2src %d, l2dst %d" 
	    (L2pkt.l2src l2pkt) d));
	  super#send_up l2pkt;
	  
      | d -> s#log_debug  (lazy
	  (Printf.sprintf "Start RX, l2src %d, l2dst %d (not for us)" (L2pkt.l2src l2pkt) d));
    end
    
  method xmit = s#frontend_xmit
    
end

type mac_queue_stats = 
    { nDrops : int } (* xxx/qmac : add more stats if necessary *)

class virtual queue_backend ?(stack=0)  ~(bps:float)
  (owner:#Node.node) =
  let myid = owner#id in
object(s)
  val pktq = Pkt_queue.create 10 (* xxx/qmac un-hardcode queue size *)

  inherit ['mac_queue_stats] backend ~stack ~bps owner as super

  method private backend_reset_stats  = Pkt_queue.reset_stats pktq

  method virtual private state : Mac.frontend_state

  method private backend_stats = { nDrops = (Pkt_queue.stats pktq).Pkt_queue.dropped} 
  method private backend_recv l2pkt = 
    let dst = L2pkt.l2dst l2pkt in
    
    (* Throw away unicast packet if not for us, keep it otherwise *)
    begin match dst with
      | d when (d = L2pkt.l2_bcast_addr) ->  s#log_debug (lazy
	  (Printf.sprintf "Start RX, l2src %d, l2dst broadcast" 
	    (L2pkt.l2src l2pkt)));
	  super#send_up l2pkt;
	  
      | d when (d = myid) ->  s#log_debug  (lazy
	  (Printf.sprintf "Start RX, l2src %d, l2dst %d" 
	    (L2pkt.l2src l2pkt) d));
	  super#send_up l2pkt;
	  
      | d -> s#log_debug  (lazy
	  (Printf.sprintf "Start RX, l2src %d, l2dst %d (not for us)" (L2pkt.l2src l2pkt) d));
    end

  method virtual private frontend_xmit : L2pkt.t -> unit

  method xmit l2pkt = ()
    (* xxx/qmac
       if the packet queue is empty, send it to the mac frontend 
       (s#frontend_xmit l2pkt).
       otherwise, add it to the queue *)

  method private backend_xmit_complete = 
    (* xxx/qmac:
       - check if any packets in queue
       - if yes, schedule an event to send it right away
    *)
    ()
end

class virtual ['stats] frontend  ?(stack=0) ~(bps:float) (owner:#Node.node) =
object
  inherit Log.virtual_loggable 

  val mutable bitsTX = 0
  val mutable bitsRX = 0
  val mutable pktsTX = 0
  val mutable pktsRX = 0

  method bps = bps

  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method virtual private frontend_xmit : L2pkt.t -> unit
  method basic_stats = {
    Mac.bits_RX = bitsRX; 
    Mac.bits_TX = bitsTX;
    Mac.pkts_RX = pktsRX; 
    Mac.pkts_TX = pktsTX
  }

  method virtual private frontend_reset_stats : unit
  method virtual private frontend_stats : 'stats

  method virtual private backend_recv : L2pkt.t -> unit
    (* this should not be implemented in the inheriting frontend mac, but
       rather in the backend which is then 'mixed in' with this frontend. it
       should be called from the recv method of the frontend. *)
end

class virtual null_frontend ?(stack=0) ~(bps:float) (owner:#Node.node) =
  let myid = owner#id in

object(s)
  inherit [unit] frontend ~stack ~bps owner as null_frontend
  val mutable state = Mac.Idle

  method private state = state

  method private frontend_reset_stats = 
    bitsTX <- 0;
    bitsRX <- 0;
    pktsRX <- 0;
    pktsTX <- 0
      
  method private frontend_stats = ()

  method private frontend_xmit l2pkt = 
    if state = Mac.Idle then (
      let xmit_done_event() = (
	assert (state = Mac.Tx);
	state <- Mac.Idle;
	s#backend_xmit_complete
      ) in
      state <- Mac.Tx;
      pktsTX <- pktsTX + 1;
      bitsTX <- bitsTX + (L2pkt.l2pkt_size ~l2pkt);
      SimpleEther.emit ~stack ~nid:myid l2pkt;

      let t = xmit_time bps l2pkt in
      (Sched.s())#sched_in ~t ~f:xmit_done_event
    )

  method recv ?(snr=1.0) ~l2pkt () = 
    if state = Mac.Idle then (
      let recv_event() = begin
	assert (state = Mac.Rx);
	state <- Mac.Idle;
	s#backend_recv l2pkt
      end in
      
      state <- Mac.Rx;
      pktsRX <- pktsRX + 1;
      bitsRX <- bitsRX + (L2pkt.l2pkt_size ~l2pkt);
      (Sched.s())#sched_in ~f:recv_event ~t:(xmit_time bps l2pkt)
    )

  method virtual private backend_xmit_complete : unit 

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
  
