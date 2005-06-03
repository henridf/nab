(*
 *
 *  nab - Network in a Box
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

(* @author Thomas Schmid. *)

open Ether
open L2pkt
open Tdack_pkt
open Printf
open Misc

type state = IDLE | WFACK | TXACK | TXPKT

let string_of_state = function
  | IDLE -> "IDLE"
  | WFACK -> "WFACK"
  | TXACK -> "TXACK"
  | TXPKT -> "TXPKT"

type stats = 
    {
      ackRX:int; (* number of received ACKs *)
      ackTX:int; (* number of transmitted ACKs *)
      ackDouble:int; (* number of double received ACKs (for one sent out message) *)
      ackTXfailed:int; (* TX failed for ACK *)
      ackCanceled:int; (* canceled scheduled send ACKs *)
      ackTimeout:int; (* counts how many times an ACK was not received after a packet was TX *)
      averageAckTime: float; (* the average time it takes until an ACK is received *)
      varianceAckTime: float; (* the variance of times it takes until an ACK is received *)
      averageAckDistance: float; (* the average distance from where we received successfull ACKs *)
      varianceAckDistance: float; (* the variance for the distance above *)
      averageRetransmissions : float; (* the average of retransmissions needed until a packet was accepted. *)
      varianceRetransmissions : float; (* the variance for the retransmissions *)
      dropRX:int; (* number of received packets which were dropped *)
      dropTX:int (* number of transmitted packets which were dropped *)
    }

(*
  This mac_queue models the following behaviour:
  - nodes can only transmit one packet at the time
  - nodes can receive at the same time from multiple neighbors
*)
  
class virtual tdack_backend ?(stack=0) ?(queuesize=10) ~(bps:float)
  (owner:#Node.node) =
  let myid = owner#id in
object(s)
  inherit [stats] Mac_base.backend ~stack ~bps owner as super

  val pktq = Pkt_queue.create queuesize 

  val mutable ackRX = 0
  val mutable ackTX = 0
  val mutable ackDouble = 0
  val mutable ackTXfailed = 0
  val mutable ackCanceled = 0
  val mutable ackTimeout = 0
  val mutable dropRX = 0
  val mutable state = IDLE

  val mutable cost = 255
  val mutable slot = 0.

  val mutable slottime = 0.
  val mutable ack_handle = 0
  val mutable ack_dst = -1
  val mutable ack_timeout_handle = 0

  val mutable pktSendTime = 0.

  val mutable num = 5   (* number of downstream neighbors *)
  val mutable min = 0    (* min cost of downstream neighbors *)
  val mutable max = 255 (* max cost of downstream neighbors *)

  val mutable currentPacket = L2pkt.ack_pkt ~src:0 ~dst:0 (* holds the packet which we transmit for retransmissions. *)
  val mutable retransmissions = 0 (* counts the number of retransmitts a packet needed until an ACK was received. *)
  val mutable maxRetransmissions = 5

  val mutable ackTimeList = [] (* this list stores all time differences between sending a
			  message and successfully receiving the ACK *)

  val mutable ackDistanceList = [] (* this list stores all distances from where we got a successfull ACK. *)

  val mutable retransmissionsList = [] (* stores the number of retransmissions needed for each packet *)

  initializer (
    (* calculate the slot time for an ACK *)
    
    let l2pkt = L2pkt.ack_pkt ~src:0 ~dst:0 in
    let bytes = (L2pkt.l2pkt_size ~l2pkt) in
    let t = (i2f (bytes * 8)) /. bps in
    slottime <- t +. 0.2 *. t    (* add 20% as guard space *)
      
  )

  method private backend_reset_stats  = 
    Pkt_queue.reset_stats pktq;
    ackRX <- 0;
    ackTX <- 0;
    ackTXfailed <- 0;
    ackCanceled <- 0;
    ackTimeout <- 0;
    dropRX <- 0;
    ackTimeList <- [];
    ackDistanceList <- [];
    retransmissionsList <- []

  method virtual private frontend_state : Mac.frontend_state

  method private backend_stats = { 
    ackRX = ackRX;
    ackTX = ackTX;
    ackDouble = ackDouble;
    ackTXfailed = ackTXfailed;
    ackCanceled = ackCanceled;
    ackTimeout = ackTimeout;
    averageAckTime = s#get_average_ack_time;
    varianceAckTime = s#get_variance_ack_time;
    averageAckDistance = s#get_average_ack_distance;
    varianceAckDistance = s#get_variance_ack_distance;
    averageRetransmissions = s#get_average_retransmissions;
    varianceRetransmissions = s#get_variance_retransmissions;
    dropRX = dropRX;
    dropTX = (Pkt_queue.stats pktq).Pkt_queue.dropped
  }


  method set_cost c =
    cost <- c;

  method get_cost () =
    cost;

  method get_slot () =
    Misc.f2i slot;

  method set_min m =
    min <- m;

  method set_max m =
    max <- m;

  method set_num n =
    num <- n;

  method private backend_recv l2pkt =  (

    let dst = L2pkt.l2dst l2pkt in
    let hdr_ext = L2pkt.l2hdr_ext l2pkt in
      
  
    (* Throw away unicast packet if not for us, keep it otherwise *)
    begin match dst with
      | d when ((d = L2pkt.l2_bcast_addr) or (d = myid)) ->  
	  (* Check for an ACK *)
	  begin match hdr_ext with
	    | TDACK ACK a ->
		begin match state with
		  | IDLE -> 
		      s#log_debug (lazy
				     (Printf.sprintf "ACK received from %d, but not waiting for ACK!"
					(L2pkt.l2src l2pkt)));
		      ackDouble <- ackDouble + 1;
		  | WFACK ->
		      s#log_debug (lazy
				     (Printf.sprintf "ACK received from %d which has cost %d."
					(L2pkt.l2src l2pkt) (a.cost)));
		      ackRX <- ackRX + 1;
		      state <- IDLE;
		      ackTimeList <- ackTimeList @ [(Time.get_time() -. pktSendTime)];
		      let d = sqrt (Coord.dist_sq ((World.w())#nodepos myid) ((World.w())#nodepos (L2pkt.l2src l2pkt))) in
			ackDistanceList <- ackDistanceList @ [d];
			(* store the number of retransmissions needed for that packet *)
			retransmissionsList <- retransmissionsList @ [Misc.i2f retransmissions];
			retransmissions <- 0;

		      (* cancel the ack timeout *)
		      assert(ack_timeout_handle <> 0);
		      (Sched.s())#cancel ack_timeout_handle;
		      ack_timeout_handle <- 0; 

		      (* we received ACK, send next message in queue *)
		      s#send_next_message ();
		  | TXACK | TXPKT ->
		      failwith (Printf.sprintf "RX in wrong state while receiving ACK for me (id: %d)!" myid)
		end
	    | TDACK PKT p ->
		begin match state with
		  | IDLE ->
		      s#log_debug (lazy
				     (Printf.sprintf "Pkt received, l2src %d, l2dst %d" 
					(L2pkt.l2src l2pkt) (L2pkt.l2dst l2pkt)));
		      (* we received a packet. Send an ACK *)
		      
		      s#schedule_send_ack ~dst:(L2pkt.l2src l2pkt)
			~min:p.Tdack_pkt.min 
			~max:p.Tdack_pkt.max 
			~num:p.Tdack_pkt.num;
			
			state <- TXACK;
		      
		      super#send_up l2pkt;

		  | WFACK ->
		      s#log_debug (lazy
				     (Printf.sprintf "Pkt received, l2src %d, l2dst broadcast but waiting for ACK!" 
					(L2pkt.l2src l2pkt)));
		      (* we silently drop that message... *)
		      dropRX <- dropRX + 1
		  | TXACK | TXPKT -> failwith ("backend_recv in wrong state after receiving broadcast packet.")
		end
	    | MACA _ | MACAW _ | NONE ->
		(* ignore *)
		()
	  end

      | d when (d = ack_dst) ->
	  if (state = TXACK) then (
	    (* the packet is for the destination we are sending an ACK to. Cancel our ACK *)
	    assert(ack_handle <> 0);
	    s#log_debug (lazy
			   (Printf.sprintf "received ACK before sending mine. Canceling send_ack"));
	    (Sched.s())#cancel ack_handle;
	    ack_handle <- 0;
	    ackCanceled <- ackCanceled + 1;
	    state <- IDLE;
	  )

      | d -> 
	  s#log_debug (lazy (Printf.sprintf "This packet is not for us"));
	  ((* this pkt is not for us. Drop it ... *) );
    end;
  )
  method virtual private frontend_xmit : L2pkt.t -> unit

  method xmit l2pkt = (
    (*   - nodes can only transmit one packet at the time:
	 if node is idle and not waiting for an ACK,
	 start transmitting this packet, otherwise, add it to the queue *) 
    if (s#frontend_state=Mac.Idle) && (state=IDLE) then (
      s#log_debug (lazy(Printf.sprintf "Tx is free: txmitting pkt!"));
      (* add the TDACK extension to the l2pkt header. the l2hdr.ext field is not mutable. therefore
      we have to create a new l2pkt. *)
      let l2pkt' = L2pkt.make_l2pkt 
	~ext: (TDACK (PKT (Tdack_pkt.make_pkt ~min ~max ~num)))
	~src:(l2src l2pkt)
	~dst:(l2dst l2pkt)
	(l3pkt l2pkt) in

	currentPacket <- l2pkt;

      state <- TXPKT;
      s#frontend_xmit l2pkt';
    )
    else(
      (* store the packet in the queue *)
      s#log_debug (lazy(Printf.sprintf "Tx is busy or waiting for ACK: storing pkt in the buffer"));
      if(Pkt_queue.push l2pkt pktq = false) then (
	(* Queue is full, dropping packet *)
	s#log_debug (lazy(Printf.sprintf "Pkt lost due to buffer overflow: Queue is FULL"));
	(Pkt_queue.stats pktq).Pkt_queue.dropped <-
	  (Pkt_queue.stats pktq).Pkt_queue.dropped + 1;
      )
    )
  )

  method private send_next_message ()= (
    (* The frontend has finished transmitting a packet:
       - check if any packets in queue
       - if yes, schedule an event to send it right away
    *)
    if not (Pkt_queue.is_empty pktq) then (
      (* take packet from the queue and send it *)
      let new_l2pkt = (Pkt_queue.pop pktq) in
	s#log_debug (lazy(Printf.sprintf "TX completed: transmitting new pkt from the buffer to node %i" (L2pkt.l2dst new_l2pkt)));
	s#xmit new_l2pkt;
    ) else 
      s#log_debug (lazy(Printf.sprintf "TX /ACK completed: no pkt to transmit in the buffer"));
  )

  method private backend_xmit_complete = (
    match state with
      | TXACK -> s#log_debug (lazy(Printf.sprintf "Tx ACK complete. Trying to send next message."));
	  state <- IDLE;
	  ack_handle <- 0;
	  ack_dst <- -1;
	  s#send_next_message ()
      | TXPKT -> s#log_debug (lazy(Printf.sprintf "Tx complete. Waiting for ACK."));
	  state <- WFACK;
	  assert(ack_timeout_handle = 0);
	  pktSendTime <- Time.get_time();
	  ack_timeout_handle <- (Sched.s())#sched_in_handle ~f:(fun () -> s#timeout_ack) ~t:((round (Misc.i2f (num+1))) *. slottime);
	  s#log_debug(lazy(Printf.sprintf "Scheduled ACK timeout"));
      | IDLE | WFACK -> failwith ("backend_xmit_complete in a state which should not be!");
  )

  method private schedule_send_ack ~dst ~min ~max ~num =
    assert (ack_handle = 0);
    ack_dst <- dst;

    let slot = round(Misc.i2f(cost - min) /. Misc.i2f(max - min) *. Misc.i2f(num)) in
      
      s#log_debug(lazy(Printf.sprintf "Scheduled ACK to send in slot %d" (Misc.f2i slot)));
      ack_handle <- (Sched.s())#sched_in_handle ~f:(fun () -> s#send_ack ~dst:dst) ~t:(slot *. slottime)

  method private send_ack ~dst = (
    (* note: ACKs should bypass the queue and never be
       put into it. *)

    (* create a dummy l3 and l4 packet FIXME: would be better to define a l2_pkt with empty l3pkt field *)
    let l4pkt = `EMPTY in
    let l3hdr = L3pkt.make_l3hdr ~src:owner#id ~dst:dst () in
    let l3pkt = L3pkt.make_l3pkt ~l3hdr ~l4pkt in
    let l2pkt = L2pkt.make_l2pkt ~ext:(TDACK (ACK {cost=0})) ~src:myid ~dst:dst l3pkt in
      
      if not(s#frontend_state=Mac.Tx) && (state=TXACK) then (
	s#log_debug (lazy(Printf.sprintf "Tx is free: txmitting ACK!"));
	s#frontend_xmit l2pkt;
	ackTX <- ackTX + 1;
      ) else(
	ackTXfailed <- ackTXfailed + 1;
	state <- IDLE;
	s#log_debug (lazy(Printf.sprintf "Mac is busy: dropping ACK"));
      )    
  ) 

  method private timeout_ack =
    (** For now, we just switch back to the idle state and count an ack timeout. *)
    assert(ack_timeout_handle <> 0);
    s#log_debug(lazy(Printf.sprintf "ACK timed out."));
    ack_timeout_handle <- 0;
    ackTimeout <- ackTimeout + 1;
    state <- IDLE;


  method private get_average_ack_time =
    s#get_average ackTimeList

  method private get_variance_ack_time =
    s#get_variance ackTimeList

  method private get_average_ack_distance =
    s#get_average ackDistanceList

  method private get_variance_ack_distance =
    s#get_variance ackDistanceList
 
  method private get_average_retransmissions =
    s#get_average retransmissionsList

  method private get_variance_retransmissions =
    s#get_variance retransmissionsList

  method private get_average l =
    Gsl_stats.mean (Array.of_list l)

      (** Calculates the sample variance (s^2) for the floats in l. *)
  method private get_variance l =
    Gsl_stats.variance (Array.of_list l)
end
