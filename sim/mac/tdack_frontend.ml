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
open Misc

let rndseed = ref 0 

type stats = 
    {
      failedRX : int;
      contention_stats : Contention_frontend.stats
    }


class virtual tdack_frontend ?(stack=0) ~bps ~chip (owner:#Node.node) = 
object(s)

  inherit Contention_frontend.contention_frontend ~stack ~bps owner as ct

  val mutable failedRX = 0
  val mutable end_tx_handle = 0

  val mutable jitter = 0.001
  val rnd = Random.State.make [|!rndseed|] 

  val mutable txPower = 0.
  val mutable ackPowerIncrease = 0.

  initializer (
    incr rndseed
  )

  (* This is called at the end (ie after the last bit comes in) of a succesful
     packet reception, ie a packet which arrived with no collisions. We have to
  differentiate between ACK and Data packets. pktsRX counts only Data pkts!*)
  method private end_rx l2pkt = (
    assert (end_rx_handle <> 0);
    end_rx_handle <- 0;
    
    (* update stats *)
    begin match (L2pkt.l2hdr_ext l2pkt) with
      | TDACK (Tdack_pkt.ACK _) -> ()
	 (* do nothing. will be counted in the backend as ackRX *)
      | TDACK (Tdack_pkt.PKT _) | MACA _ | MACAW _ | NONE ->
	  pktsRX <- pktsRX + 1
    end;
    
    bitsRX <- bitsRX + (L2pkt.l2pkt_size ~l2pkt);
    
    assert (interfering_until = Time.get_time());

    s#backend_recv l2pkt

  )


  method recv ?(snr=1.0) ~l2pkt () = (

    let end_rx_time = 
      Time.get_time()
      +. s#xmitdelay l2pkt in

    let error_probability = Channel_model.pe ~modulation:chip.Radiochips.modulation 
      ~gamma:snr ~b_n:chip.Radiochips.b_n ~rate:chip.Radiochips.rate in


      (*Printf.printf "size: %d\n" (L2pkt.l2pkt_size ~l2pkt:l2pkt);*)
    let packet_rx_proba = Channel_model.packet_reception_probability 
      ~encoding:chip.Radiochips.encoding ~framelength:(L2pkt.l2pkt_size ~l2pkt:l2pkt) 
      ~preamblelength:4 ~pe:error_probability in

    let instantanious_snr = Random.float 1. in
    if instantanious_snr <= packet_rx_proba then (
      ct#recv ~snr ~l2pkt ();
     ) else (
      s#log_debug (lazy (
		     sprintf "RX faild for Pkt from %d, instantanious SNR %.3f, receive probability %.3f error_probability:%.3f snr:%.3f"
		       (l2src l2pkt) instantanious_snr packet_rx_proba error_probability snr));
      failedRX <- failedRX + 1;
      (* check if we were rx or tx... *)
      begin match s#sending, s#interfering with
	| true, _ -> collsRXTX <- collsRXTX + 1
	| _, true ->
	    if (end_rx_handle <> 0) then begin
	      (Sched.s())#cancel end_rx_handle;
	      end_rx_handle <- 0
	    end;
	    collsRXRX <- collsRXRX + 1;
	    s#log_debug (lazy (
			   sprintf "Pkt from %d collided with already receiving packet from %d"
			     (l2src l2pkt) receiving_from))
	| _, _ -> ();
      end;

      (* We will be under interference for the duration of this packet.*)
      interfering_until <- max interfering_until end_rx_time
     );
  )

  method virtual private backend_xmit_complete : unit
    (* should be implemented by the backend which will then get mixed in with
       this frontend *)

  (* overrides the method #final_xmit from contention_frontend *)
  method private final_xmit l2pkt = (
    
    begin match (L2pkt.l2hdr_ext l2pkt) with
      | TDACK (Tdack_pkt.ACK _) -> 
	  (* do nothing. has been counted in backend *)
	  let ackTXPower = txPower +. ackPowerIncrease in
	    (Ether.emit ()) ~pt:ackTXPower ~pn:chip.Radiochips.p_n ~stack ~nid:(owner#id) l2pkt
      | TDACK (Tdack_pkt.PKT _) | MACA _ | MACAW _ | NONE ->
	  pktsTX <- pktsTX + 1;
	  (Ether.emit ()) ~pt:txPower ~pn:chip.Radiochips.p_n ~stack ~nid:(owner#id) l2pkt
    end;
    
    bitsTX <- bitsTX + (L2pkt.l2pkt_size ~l2pkt);
  )

  method private tdack_stats = 
    {
      failedRX = failedRX;
      contention_stats = ct#frontend_stats
    }
      
  method private tdack_reset_stats = (
    failedRX <- 0;
    ct#frontend_reset_stats
  )

  method set_tx_power p =
    txPower <- p

  method set_ack_power_increase p =
    ackPowerIncrease <- p
 

end

let string_of_ostats s = 
  Printf.sprintf
    "%s, %d RX failed"
    (Contention_frontend.string_of_ostats s.contention_stats)
    s.failedRX

let add_ostats s1 s2 =
  {
   failedRX = s1.failedRX + s2.failedRX;
   contention_stats = Contention_frontend.add_ostats s1.contention_stats s2.contention_stats
  }


let zero_ostats () = 
  {
   failedRX = 0;
   contention_stats = (Contention_frontend.zero_ostats ())
  }
