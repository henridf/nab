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




(** 
  A MAC layer with modeling of collisions.
  This MAC can either be sending or receiving.
  If a packet is received while already receiving, both are lost
  (collision). Collisions are detected instantaneously; the node immediately
  stops receiving. Therefore a node can send a packet right after the
  beginning of a collision.
  
  If a packet is received while sending, sending continues ok but reception
  fails.
  If node tries to send while either sending or receiving, the packet to be
  sent is dropped silently.
  
  No queuing/buffering of packets.

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

class contentionmac :
  ?stack:int ->
  float ->
  #Simplenode.simplenode ->
  object
    inherit Mac.t
    method other_stats : stats
    method set_jitter : float -> unit
  end

val macs : ?stack:int -> unit -> (Common.nodeid_t, contentionmac) Hashtbl.t 
  (** Returns a hashtbl, indexed by {!Common.nodeid_t}, of all the mac objects
    for stack [stack] (stack defaults to 0 if argument not provided). 
    If there are no macs on this stack, returns an empty hashtbl.
  *)

val mac :  ?stack:int -> Common.nodeid_t -> contentionmac
  (** Returns the contentionmac mac object for given node on given stack.
    @raise Not_found if there is no contentionmac on this stack/node pair.
  *)





(** {1 Functions for manipulating {!Mac_contention.stats} .} *) 

val string_of_ostats : stats -> string
  (** Return a string representation of a {!Mac_contention.stats}. *)

val add_ostats : stats -> stats -> stats
  (** Add two {!Mac_contention.stats}, field by field. *)

val zero_ostats : unit -> stats
  (** Return a {!Mac_contention.stats} with all values initialized to 0. *)
