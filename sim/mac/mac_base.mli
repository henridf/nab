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
val mac : ?stack:int -> Common.nodeid_t -> Mac.t


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



(** Return string representations of basic_stats. *) 

val string_of_bstats : Mac.basic_stats -> string
val string_of_bstats_bits : Mac.basic_stats -> string
val string_of_bstats_pkts : Mac.basic_stats -> string


val add_bstats : Mac.basic_stats -> Mac.basic_stats -> Mac.basic_stats
  (** Add two basic_stats. *)


val zero_bstats : unit -> Mac.basic_stats
  (** Return a basic_stats with all values initialized to 0. *)
