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



(** A MAC backend (see {!Mac_base.backend}) It models the ACK schema and
    logic for sending ACKs. After sending a packet, the sender waits for an ACK
    from a node which successfully received the message. If no ACK is received,
    a timer will timeout and the message will be counted as not acknowledged.
    The time after a message is devided into time slots, since multiple nodes
    can receive and acknowledge a packet. Each node then tries to deliver its
    ACK in one of these slots.

  @author Thomas Schmid
*)

type state = IDLE | WFACK | TXACK | TXPKT
(** The different states are:
    IDLE: the module is waiting for incoming ACKs or PKTs
    WFACK: the module is waiting for an ACK
    TXACK: the module is transmitting an ACK
    TXPKT: the module is transmitting a PKT
*)

type stats = {
  ackRX : int;
  ackTX : int;
  ackDouble : int;
  ackTXfailed : int;
  ackCanceled : int;
  ackTimeout : int;
  averageAckTime: float;
  varianceAckTime: float;
  averageAckDistance: float;
  varianceAckDistance: float;
  averageRetransmissions : float;
  varianceRetransmissions : float;
  dropRX : int;
  dropTX : int;
}
(** The statistics for the backend are:
    ackRX: number of ACKs transmitted
    ackTX: number of ACKs successfuly received
    ackDouble: number of ACKs which were received after a successful ACK
    ackTXfailed: number of ACKS which failed to transmit
    ackCanceled: number of ACKs which were cancelled because of overheard ACKs
    ackTimeout: number of timeouts received because no ACK arrived
    averageAckTime: sample average time until an ACK was successfully received
    varianceAckTime: sample variance for the above
    averageAckDistance: sample average for distance from which ACK was received
    varianceAckDistance: sample variance for the above
    averageRetransmissions: the average of retransmissions needed until a packet was accepted.
    varianceRetransmissions: the variance for the retransmissions
    dropRX: number of packets which had to be dropped for RX
    dropTX: number of packets which hat to be dropped because queue is full
*)

(**
   This backend implements the ACK logic. Messages are received from the upper
   layer and handed down to the frontend. After a packet was sent successfuly, 
   the module waits until an ACK is received or a timeout occured.
*)
class virtual tdack_backend :
  ?stack:int ->
  ?queuesize:int ->
  bps:float ->
  #Node.node ->
object
  inherit Log.virtual_loggable
  inherit [stats] Mac_base.backend

  method private backend_recv : L2pkt.t -> unit
  method private backend_reset_stats : unit
  method private backend_stats : stats
  method private backend_xmit_complete : unit
  method private virtual frontend_state : Mac.frontend_state
    
  method private virtual frontend_xmit : L2pkt.t -> unit
    
  method set_cost : int -> unit
    (** Sets the cost for this node *)
  method get_cost : unit -> int
    (** Returns the cost for this node *)
  method get_slot : unit -> int
    (** Returns the slot calculated from the cost, min, max and num parameters *)
  method set_max : int -> unit
    (** Sets the max cost for downstream neighbors *)
  method set_min : int -> unit
    (** Sets the min cost for downstream neighbors *)
  method set_num : int -> unit
    (** Sets the number of downstream neighbors *)
    
  method xmit : L2pkt.t -> unit
    (** Sends out a L2 packet to the frontend. TDACK specific header information is added here. *)
    
end
  

val string_of_state : state -> string

