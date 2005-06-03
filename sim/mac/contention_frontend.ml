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


open L2pkt
open Printf
open Misc

let rndseed = ref 0 

type stats = 
    {collsRXRX : int;
    collsRXTX : int;
    dropsTXTX : int;
    dropsTXRX : int
    }


class virtual contention_frontend ?(stack=0) ~bps (owner:#Node.node) = 
object(s)

  inherit [stats] Mac_base.frontend ~stack ~bps owner

  val mutable interfering_until = Time.get_time() -. 1.
  val mutable sending_until = Time.get_time() -. 1.
  val mutable receiving_from = 0

  val mutable collsRXRX = 0
  val mutable collsRXTX = 0
  val mutable dropsTXTX = 0
  val mutable dropsTXRX = 0

  val mutable end_rx_handle = 0
  val mutable end_tx_handle = 0

  val rnd = Random.State.make [|!rndseed|] 
    
  val mutable jitter = 0.001

  val myid = owner#id

  (*
    sending -> interfering
    receiving -> interfering   
    interfering -> cannot receive 
  *)

  (* True if any packet transmission is ongoing in our radio
     range. (including if we are the transmitter)
  *)
  method private interfering =  
    interfering_until >= Time.get_time()

  (* True if we are actively sending a packet. This flag remains true until
     transmission of packet is complete. *)
  method private sending = 
    sending_until >= Time.get_time()

  method private frontend_state = 
    if s#sending then Mac.Tx else if end_rx_handle <> 0 then Mac.Rx else Mac.Idle


  (* This is called at the end (ie after the last bit comes in) of a succesful
     packet reception, ie a packet which arrived with no collisions. *)
  method private end_rx l2pkt = (
    assert (end_rx_handle <> 0);
    end_rx_handle <- 0;
    
    (* update stats *)
    pktsRX <- pktsRX + 1;
    bitsRX <- bitsRX + (L2pkt.l2pkt_size ~l2pkt);
    
    assert (interfering_until = Time.get_time());

    s#backend_recv l2pkt

  )

  method private xmitdelay l2pkt = 
    let bytes = (L2pkt.l2pkt_size ~l2pkt) in
    (i2f (bytes * 8)) /. bps

  (* This is called when the first bit of a packet lands at this MAC. *)

  method recv ?(snr=1.0) ~l2pkt () = (
    let end_rx_time = 
      Time.get_time()
      +. s#xmitdelay l2pkt in

    begin match s#sending, s#interfering with 
      | false, false ->
	  (* Not xmiting and not under interference. 
	     -> Schedule the full packet reception event.*)
	  s#log_debug (lazy (sprintf "RX packet of (%d bytes)" (l2pkt_size ~l2pkt)));
 	  receiving_from <- l2src l2pkt;
	  let recv_event() =  s#end_rx l2pkt  in
	  end_rx_handle <- (Sched.s())#sched_at_handle ~f:recv_event ~t:(Scheduler.Time end_rx_time);
	  
      | true, _ ->  collsRXTX <- collsRXTX + 1
      | _, true -> 
	  if (end_rx_handle <> 0) then begin
	    (Sched.s())#cancel end_rx_handle;
	    end_rx_handle <- 0
	  end;
	  collsRXRX <- collsRXRX + 1;
	  s#log_debug (lazy (
	    sprintf "Pkt from %d collided with already receiving packet from %d"
	    (l2src l2pkt) receiving_from))
    end;
    
    (* We will be under interference for the duration of this packet. *)
    interfering_until <- max end_rx_time interfering_until
  )

  method set_jitter j = jitter <- j

  method virtual private backend_recv : L2pkt.t -> unit
    (* should be implemented by the backend which will then get mixed in with
       this frontend *)

  method virtual private backend_xmit_complete : unit
    (* should be implemented by the backend which will then get mixed in with
       this frontend *)

  method private frontend_xmit l2pkt = (
      let receiving = end_rx_handle <> 0 in
      if not s#sending && not receiving then (
	s#log_debug (lazy (sprintf "TX packet (%d bytes)" (l2pkt_size ~l2pkt)));
	let delay = Random.State.float rnd jitter in
	let end_xmit_time = 
	  Time.get_time() 
	  +. s#xmitdelay l2pkt 
	  +. delay in

	  sending_until <- end_xmit_time;
	  interfering_until <- max end_xmit_time interfering_until;


	  let end_tx_event()  = s#backend_xmit_complete in
	    (* notify the backand of the end of xmit *)
	    end_tx_handle <- (Sched.s())#sched_at_handle ~f:end_tx_event ~t:(Scheduler.Time end_xmit_time);
	    s#log_debug (lazy (sprintf "Switching from RX to TX, final xmit delayed by %f" delay));
	    
	  (Sched.s())#sched_in ~f:(fun () -> s#final_xmit l2pkt) ~t:delay;
 
     ) else (
	let dst_str = (string_of_l2dst (l2dst l2pkt)) in
	if s#sending then (
	  s#log_info (lazy (sprintf "Pkt to %s dropped because already sending"
	    dst_str));
	  dropsTXTX <- dropsTXTX + 1;
	) else (
	  s#log_info (lazy (sprintf "Pkt to %s dropped because already receiving"
	    dst_str));
	  dropsTXRX <- dropsTXRX + 1;
	)
      )
  )

  method private final_xmit l2pkt = (
	
      pktsTX <- pktsTX + 1;
      bitsTX <- bitsTX + (L2pkt.l2pkt_size ~l2pkt);

      (Ether.emit ()) ~stack ~nid:myid l2pkt
   )

  method basic_stats = {
    Mac.bits_RX = bitsRX; 
    Mac.bits_TX = bitsTX;
    Mac.pkts_RX = pktsRX; 
    Mac.pkts_TX = pktsTX
  }

  method private frontend_stats = 
    {
      collsRXRX = collsRXRX; 
      collsRXTX = collsRXTX;
      dropsTXTX = dropsTXTX; 
      dropsTXRX = dropsTXRX
    }

  method private frontend_reset_stats = 
    bitsTX <- 0; 
    bitsRX <- 0;
    pktsTX <- 0; 
    pktsRX <- 0;
    collsRXRX <- 0;
    collsRXTX <- 0;
    dropsTXTX <- 0;
    dropsTXRX <- 0

end

let string_of_ostats s = 
  Printf.sprintf
    "%d RX/RX colls, %d RX/TX colls, %d TX/TX drops, %d TX/RX drops"
    s.collsRXRX
    s.collsRXTX
    s.dropsTXTX
    s.dropsTXRX


let add_ostats s1 s2 =
  {
    collsRXRX = s1.collsRXRX + s2.collsRXRX;
    collsRXTX = s1.collsRXTX + s2.collsRXTX;
    dropsTXTX = s1.dropsTXTX + s2.dropsTXTX;
    dropsTXRX = s1.dropsTXRX + s2.dropsTXRX
  }


let zero_ostats () = 
  {
    collsRXRX = 0;
    collsRXTX = 0;
    dropsTXTX = 0;
    dropsTXRX = 0
  }
