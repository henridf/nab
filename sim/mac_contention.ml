(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  Contention MAC: A simple MAC layer with high level collision modeling.
  This MAC can either be sending or receiving.
  If a packet is received while already received, both are dropped
  (collision).
  If a packet is received while sending, sending continues ok but reception
  fails.
  If node tries to send while either sending or receiving, the packet to be
  sent is dropped silently.
  
  No queuing/buffering of packets.

  @author Henri Dubois-Ferriere.
*)


open Ether
open L2pkt
open Printf

let rndseed = ref 0 

class contentionmac owner : Mac.mac_t = 
object(s)

  inherit Log.inheritable_loggable

  val ownerid = owner#id
  val owner:#Simplenode.simplenode = owner

  val mutable collision = false
  val mutable interfering_until = Common.get_time()
  val mutable sending_until = Common.get_time()
  val mutable receiving_from = 0
  val mutable receiving = false
  val rnd = Random.State.make [|!rndseed|] 

  initializer (
    s#set_objdescr ~owner:(owner :> #Log.inheritable_loggable)  "/cmac";
    incr rndseed
  )

  (*
    sending -> interfering
    receiving -> interfering   
    interfering -> cannot receive 
  *)

  (* Check if a packet transmission is ongoing in our radio range.
     This could be us transmitting, or a neighbor transmitting.  *)
  method private interfering =  
    interfering_until >= Common.get_time()
      
  (* Check if we are actively sending a packet*)
  method private sending = 
    sending_until >= Common.get_time()

  method private end_rx l2pkt = (
    assert (receiving);
    receiving <- false;
    if collision then 
      collision <- false
    else 
      begin
	assert (interfering_until = Common.get_time());
	let dst = l2dst ~pkt:l2pkt in
	if (dst = L2_BCAST || dst = L2_DST ownerid) then 
	  owner#mac_recv_pkt ~l2pkt
      end
  )

  method recv ?snr ~l2pkt () = (

    (* If we're already in rx, set the collision flag so that when
       we know to drop the packet when ongoing rx completes. *)
    if receiving then (
	collision <- true;
	s#log_notice (lazy 
	  (sprintf "Pkt from %d collided with already receiving  packet for %d"
	    (l2src ~pkt:l2pkt) receiving_from))
    ) else (
      assert (collision = false);

      (* if we're not in tx/rx, and not already under interference, then mark that 
	 we're receiving, and schedule the full packet reception event. *)
      if  not s#sending && not s#interfering then (
	receiving <- true;
	s#log_debug (lazy (sprintf "RX packet of (%d bytes)" (l2pkt_size ~l2pkt)));
	
	let end_rx_time = 
	  Common.get_time() 
	  +. xmitdelay ~bytes:(L2pkt.l2pkt_size ~l2pkt:l2pkt) in

	receiving_from <- l2src ~pkt:l2pkt;

	let recv_event() =  s#end_rx l2pkt  in
	(Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time end_rx_time);

	(* Finally, mark that we're being interfered for the duration of this packet.*)
	interfering_until <- max end_rx_time interfering_until;
      )
    )
  )


  method xmit ~l2pkt = 

    if (Random.State.int rnd 2) = 1 then (
      let delay = Random.State.float rnd 0.1 in
      (Gsched.sched())#sched_in ~f:(fun () -> s#xmit ~l2pkt) ~t:delay;
      s#log_debug (lazy (sprintf "Delayed xmit by %f" delay))
    )
    else (
      if not s#sending && not receiving then (
	s#log_debug (lazy (sprintf "TX packet (%d bytes)" (l2pkt_size ~l2pkt)));
	let end_xmit_time = 
	  Common.get_time() 
	  +. xmitdelay ~bytes:(L2pkt.l2pkt_size ~l2pkt:l2pkt) in
	sending_until <- end_xmit_time;
	interfering_until <- max end_xmit_time interfering_until;
	Ether.emit ~nid:ownerid ~l2pkt
      ) else (
	let msg = 
	  if s#sending then "sending" else  "receiving" 
	in 
	s#log_notice (lazy (sprintf "Pkt to %s dropped because already %s" 
	  (string_of_l2dst (l2dst l2pkt))
	  msg))
      )
    )
end
      
    
    
