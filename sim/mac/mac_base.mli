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



(** Base class for MAC layers. It is recommended to inherit from
  this class when implementing a new MAC layer.

  This class essentially serves to hide multistack details (see {!Simplenode.simplenode}
  for an explanation of multiple stacks) from the derived
  subclasses, to avoid having to keep track of which stack an agent is in
  (since in most cases nodes are run with only one stack).

  The class also specifies virtual methods which must be implemented in order
  to conform to the {!Mac.t} interface.

  @author Henri Dubois-Ferriere.
*)


val macs : ?stack:int -> unit -> (Common.nodeid_t, Mac.t) Hashtbl.t
  (** Returns a hashtbl, indexed by {!Common.nodeid_t}, of all the mac objects
    for stack [stack] (stack defaults to 0 if argument not provided). *)

val mac : ?stack:int -> Common.nodeid_t -> Mac.t
  (** Returns the mac object for given node on given stack.
    The mac object is coerced down to a {!Mac.t}, so even if the underlying
    mac is a "more specialized" one (ie, a {!Mac_contention.contentionmac}),
    any additional methods beyond those defined in {!Mac.t} will be hidden. If
    you need access to those methods, get the object directly from the module
    where it is defined  (e.g., in {!Mac_contention.mac}). *)
    

(** @param stack serves to distinguish the stack when multiple stacks are being
  used. Defaults to 0. (The notion of multiple stacks is explained in
  {!Simplenode.simplenode}). 
  @param bps speed in bits per sec of this interface.
  @param simplenode the node on which this MAC layer is running.
*)
class virtual ['a] base : ?stack:int -> bps:float -> #Simplenode.simplenode ->
object
  inherit Log.inheritable_loggable 

  val myid : Common.nodeid_t

  (** bitsTX and bitsRX are counters representing the total # bits transmitted/received by this
    MAC; they should be incremented appropriatly by the inheriting class. *)
  val mutable bitsTX : int 
  val mutable bitsRX : int
  val mutable pktsRX : int 
  val mutable pktsTX : int

  (** Compute the transmission delay for a packet, given packet size and xxx bps*)
  method private xmitdelay : L2pkt.t -> float
  method private send_up : l2pkt:L2pkt.t -> unit
  method virtual other_stats : 'a

  (** Methods documented in {!Mac.t}. *)
  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method virtual xmit : l2pkt:L2pkt.t -> unit
  method virtual bps : float
  method basic_stats : Mac.basic_stats
  method reset_stats : unit
end



(** {1 Functions for manipulating {!Mac.basic_stats} .} *) 

val string_of_bstats : Mac.basic_stats -> string
  (** Return a string representation of a {!Mac.basic_stats}. *)

val string_of_bstats_bits : Mac.basic_stats -> string
  (** Return a string representation of a {!Mac.basic_stats}, but with only
    the fields that count bits [bits_TX] and [bits_RX]. *)

val string_of_bstats_pkts : Mac.basic_stats -> string
  (** Return a string representation of a {!Mac.basic_stats}, but with only
    the fields that count packets [pkts_TX] and [pkts_RX]. *)

val add_bstats : Mac.basic_stats -> Mac.basic_stats -> Mac.basic_stats
  (** Add two {!Mac.basic_stats}, field by field. *)

val zero_bstats : unit -> Mac.basic_stats
  (** Return a {!Mac.basic_stats} with all values initialized to 0. *)
