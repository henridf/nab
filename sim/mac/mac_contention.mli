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

(** A MAC layer with a contention frontend and a null backend, ie
  packets from the upper layers are sent directly to the frontend, and
  vice-versa.

  No queuing/buffering of packets - if a packet is received from the upper
  layer when already transmitting, it is dropped.
  No retransmissions or link-layer acknowledgements. *)
class contentionmac :
  ?stack:int ->
  bps:float ->
  #Node.node ->
  object
    inherit Mac.t
    method other_stats : Contention_frontend.stats
    method set_jitter : float -> unit
      (** Set the jitter value (in seconds) of this mac object.
	When a [contentionmac] is handed a packet to be transmitted, it waits 
	a random time, uniformly chosen between 0 and [jitter] seconds before
	transmitting. The default jitter value is 0.1 seconds. *)
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


