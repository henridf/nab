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

let rndseed = ref 0 

type stats = 
    {collsRXRX : int;
    collsRXTX : int;
    dropsTXTX : int;
    dropsTXRX : int
    }


let macs_array_ = 
  Array.init Simplenode.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i


class contentionmac ?(stack=0) bps owner  = 
object(s)

  inherit [stats] Mac_base.base ~stack ~bps owner as super

  val mutable interfering_until = Time.get_time() -. 1.
  val mutable sending_until = Time.get_time() -. 1.
  val mutable receiving_from = 0
  val rnd = Random.State.make [|!rndseed|] 

  val mutable collsRXRX = 0
  val mutable collsRXTX = 0
  val mutable dropsTXTX = 0
  val mutable dropsTXRX = 0

  val mutable end_rx_handle = 0
  val mutable jitter = 0.1

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/cmac";
    Hashtbl.replace macs_array_.(stack) owner#id (s :> contentionmac);
    incr rndseed
  )

  method bps = bps

  method reset_stats = 
    collsRXRX <- 0;
    collsRXTX <- 0;
    dropsTXTX <- 0;
    dropsTXRX <- 0;
    super#reset_stats

  method set_jitter j = jitter <- j

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

  (* This is called at the end (ie after the last bit comes in) of a succesful
     packet reception, ie a packet which arrived with no collisions. *)
  method private end_rx l2pkt = (
    assert (end_rx_handle <> 0);
    end_rx_handle <- 0;
    
    (* update stats *)
    pktsRX <- pktsRX + 1;
    bitsRX <- bitsRX + (L2pkt.l2pkt_size ~l2pkt);

    assert (interfering_until = Time.get_time());
    let dst = l2dst l2pkt in
    if (dst = l2_bcast_addr || dst = myid) then 
      super#send_up ~l2pkt
  )

  (* This is called when the first bit of a packet lands at this MAC.*)
  method recv ?snr ~l2pkt () = (

    let end_rx_time = 
      Time.get_time()
      +. super#xmitdelay l2pkt in

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


  method xmit ~l2pkt = 
    let delay = Random.State.float rnd jitter in
    (Sched.s())#sched_in ~f:(fun () -> s#really_xmit ~l2pkt) ~t:delay;
    s#log_debug (lazy (sprintf "Delayed xmit by %f" delay))

  method private really_xmit ~l2pkt = (
      let receiving = end_rx_handle <> 0 in
      if not s#sending && not receiving then (
	s#log_debug (lazy (sprintf "TX packet (%d bytes)" (l2pkt_size ~l2pkt)));
	let end_xmit_time = 
	  Time.get_time() 
	  +. super#xmitdelay l2pkt in
	sending_until <- end_xmit_time;
	interfering_until <- max end_xmit_time interfering_until;
	
	pktsTX <- pktsTX + 1;
	bitsTX <- bitsTX + (L2pkt.l2pkt_size ~l2pkt);
	
	SimpleEther.emit ~stack ~nid:myid l2pkt
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

	
  method other_stats = 
    {collsRXRX = collsRXRX; 
    collsRXTX = collsRXTX;
    dropsTXTX = dropsTXTX; 
    dropsTXRX = dropsTXRX
    }
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
    collsRXRX = s1.collsRXRX + s1.collsRXRX;
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
