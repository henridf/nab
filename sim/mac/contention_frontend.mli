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




(** A MAC frontend (see {!Mac_base.frontend}) with modeling of contention and collisions. 

  @author Henri Dubois-Ferriere.
*)

type stats = 
    {collsRXRX : int;
    collsRXTX : int;
    dropsTXTX : int;
    dropsTXRX : int
    }
    (** Statistics maintained by [mac_contention] MAC layer 
      (in addition to statistics from {!Mac.basic_stats}).
      - [collsRXRX] counts the number of "receive on receive" collisions,
      ie a packet arrives when we are already receiving another packet
      (causing both to be dropped).
      - [collsRXTX] counts the number of "receive on send" collisions, ie a
      packet arrives when we are already sending a packet (causing the
      incoming packet to be dropped).
      - [dropsTXTX] counts the number of "send on send" packet drops, ie a
      packet to send is handed down from upper layers when a packet
      transmission is ongoing.
      - [dropsTXRX] counts the number of "send on receive" packet drops, ie a
      packet to send is handed down from upper layers when a packet reception
      is ongoing.
    *)

(** The contention frontend virtual class. 
  This MAC can either be sending or receiving.
  If a packet is received while already receiving, both are lost
  (collision). Collisions are detected instantaneously; the node immediately
  stops receiving. Therefore a node can send a packet right after the
  beginning of a collision.
  
  If a packet is received while sending, sending continues ok but reception
  fails.
  If node tries to send while either sending or receiving, the packet to be
  sent is dropped silently.
*)
class virtual contention_frontend : ?stack:int -> bps:float ->
  #Node.node -> 
object
  inherit Log.virtual_loggable 
  inherit [stats] Mac.frontend_t

  (* Instance vars that are exported for inheriting classes. *) 
  val mutable interfering_until : Time.time_t
  val mutable sending_until : Time.time_t
  val mutable receiving_from : int
  val mutable collsRXTX : int
  val mutable collsRXRX : int
  val mutable pktsTX : int
  val mutable pktsRX : int
  val mutable bitsTX : int
  val mutable bitsRX : int
  val mutable end_rx_handle : int

  (** Set max initial TX backoff (effective backoff is uniform random
    variable between 0 and this value).
  *)
  method set_jitter : float -> unit
    
  method private end_rx : L2pkt.t -> unit
  method private frontend_state : Mac.frontend_state
  method private interfering : bool
  method private sending : bool
  method private final_xmit : L2pkt.t -> unit
end


(** {1 Functions for manipulating {!Contention_frontend} .} *) 

val string_of_ostats : stats -> string
  (** Return a string representation of a {!Contention_frontend}. *)

val add_ostats : stats -> stats -> stats
  (** Add two {!Contention_frontend}, field by field. *)

val zero_ostats : unit -> stats
  (** Return a {!Contention_frontend} with all values initialized to 0. *)
