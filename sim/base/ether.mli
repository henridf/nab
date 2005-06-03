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
  Ether: The shared medium onto which nodes transmit. 
  @author Henri Dubois-Ferriere 
*)

val speed_of_light : float
val propdelay : Coord.coordf_t -> Coord.coordf_t -> float
  (** Computes the propagation delay (at light speed) between 
    two coordinates (expressed in meters). *)

(** Available Ether models *)
type ether_t = SimpleEther   (** see {!Ether.SimpleEther}. *)
	       | LossyEther  (** see {!Ether.LossyEther}. *)
	       | NullEther   (** see {!Ether.NullEther}. *)

val set_ether : ether_t -> unit 
  (** Choose the type of ether used in the simulation (default is
    SimpleEther). *)


module type Ether_t = sig 
  val emit : ?pt: float -> ?pn: float -> stack:int -> nid:Common.nodeid_t -> L2pkt.t -> unit 
end

module SimpleEther : Ether_t
  (** SimpleEther is a shared medium where a packet is propagated intact to
    all nodes within radio range {!Params.radiorange} of the emitting node. *)

module NullEther : Ether_t
  (** NullEther is identical to SimpleEther except that unicast packets are
    only propagated to their destination. This can be used in conjunction
    with a MAC layer which does not model contention (for example
    {!Mac_null.nullmac}) for a slight performance gain. 
    This should *not* be used with a MAC layer that does model contention
    (since the mac layer would not know about the contention occuring
    when a neighbor unicasts a packet to another node).
*)
  
module LossyEther : Ether_t
    (** LossyEther is identical to SimpleEther but it adds a reception probability
      * to each recv (in the SNR field), which is computed using the
      * [Channel_model] module. The Mac layer can then decide if it was able
      * to decode the message or not. 
    *)

val emit : unit -> ?pt: float -> ?pn: float -> stack:int -> nid:Common.nodeid_t -> L2pkt.t -> unit
  (** A node's MAC calls this to emit bits into the air. The Ether module
    then takes care of sending them, with appropriate propagation delay and SNR,
    to nodes within range. 
    [stack] serves to distinguish when multiple stacks are being used. (The
    notion of multiple stacks is explained in {!Node.node}). 
    [nid] is the id of the sending node.
    [pt] is the transmit power
    [pn] is the noise floor of the used radio chip
  *)
