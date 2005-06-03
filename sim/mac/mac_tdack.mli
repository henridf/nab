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




(** A MAC with modeling of contention, collisions and lossy links. 

    The distinctive feature of this MAC protocol is that multiple neighbors
    can acknowledge a message, i.e. all neighbors, which successfully
    decode the message, can send an ACK. The preference is given to
    noes which have a lower cost. It is to note that this cost is not determined
    by the MAC layer. It is given from the routing layer (or a genie) which
    has a better overview of the network structure.

  @author Thomas Schmid.
*)

type stats = {
  collsRXRX : int;
  collsRXTX : int;
  dropsTXTX : int;
  dropsTXRX : int;
  failedRX : int;
  ackRX:int;
  ackTX:int;
  ackDouble:int;
  ackTXfailed:int;
  dropRX:int;
  dropTX:int
}

(** A MAC layer with a tdack frontend and a tdack backend.
*)
class tdackmac :
  ?stack:int ->
  bps:float ->
  chip:Radiochips.t ->
  #Node.node ->
  object
    inherit Mac.t
    method other_stats : Tdack_frontend.stats
    method set_jitter : float -> unit
      (** Set the jitter value (in seconds) of this mac object.
	When a [tdackmac] is handed a packet to be transmitted, it waits 
	a random time, uniformly chosen between 0 and [jitter] seconds before
	transmitting. The default jitter value is 0.1 seconds. *)

    method set_tx_power : float -> unit
      (** Set the tx power. *)
    method set_ack_power_increase : float -> unit
      (** Set the power increase with which an ACK is sent *)
    method reset_stats : unit
      (** Reset all statistics to 0 (backend, frontend and basic stats) *)

    method set_cost : int -> unit
    method get_cost : unit -> int
    method get_slot : unit -> int
    method set_min : int -> unit
    method set_max : int -> unit
    method set_num : int -> unit

    method backend_stats : Tdack_backend.stats
      (** Returns the statistics for the backend module. *)
  end

val macs : ?stack:int -> unit -> (Common.nodeid_t, tdackmac) Hashtbl.t 
  (** Returns a hashtbl, indexed by {!Common.nodeid_t}, of all the mac objects
    for stack [stack] (stack defaults to 0 if argument not provided). 
    If there are no macs on this stack, returns an empty hashtbl.
  *)

val mac :  ?stack:int -> Common.nodeid_t -> tdackmac
  (** Returns the tdackmac mac object for given node on given stack.
    @raise Not_found if there is no contentionmac on this stack/node pair.
  *)


