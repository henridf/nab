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
  MAC layer interfaces and helper functions. One can either implement a full
  MAC layer in a single class (of type {!Mac.t}), or mix together a frontend (of
  type {!Mac.frontend_t}) and a backend (of type {!Mac.backend_t}).

  @author Henri Dubois-Ferriere.
*)

type basic_stats = 
    { 
      pkts_RX : int;
      bits_RX : int;
      pkts_TX : int;
      bits_TX : int}
    (** The stats every MAC layer implementation is required to maintain. As
      basic as it gets.  Of course, most MACs will want to maintain other
      stats as well - this is not specified in the common MAC interface, since
      the specific stats will vary depending on the characteristics of the
      MAC. 
      In every mac implementation, the [bits_RX] and [bits_TX] fields should
      count the total number of bits received/transmitted.
      The meaning of the [pkts_RX] and [pkts_TX] fields may vary for different
      MAC layers. For example, a mac layer may choose to increment these
      counters for every packet received (including short control packets), or
      only for data packets. This should be specified in the documentation of
      each MAC layer.
    *)

(**  
  The complete interface which a MAC layer must implement.

  Note for those implementing a MAC layer: it is simplest to inherit
  from {!Mac_base.base}, for the reasons described therein.
  For a simple example of a MAC class, see {!Mac_null.nullmac}.
*)
class type t  = 
object

  inherit Log.inheritable_loggable 

  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
    (** [recv] is called when a packet arrives at a node.

      More precisely, [recv] is called when the {i first bit} of a packet
      arrives at the node. So, a realistic MAC implementation should wait an
      appropriate duration (depending on packet size and transmission rate of
      this MAC). A MAC that detects collisions would also keep track of the
      fact that it is receiving for this duration, in order to know that there
      is a collision if another packet is received in this interval.
    *)
      
  method xmit : L2pkt.t -> unit
    (** [xmit] is called by the upper layers on the node containing this MAC
      to send a packet out. *)

  method bps : float
    (** Return this MAC's bps speed. *)
  method basic_stats : basic_stats
    (** Return stats for this MAC object. *)

  method reset_stats : unit
    (** Reset stats for this MAC object. *)

end
    
(** The type of a MAC backend. *)
class type ['stats] backend_t = 
object
    method xmit : L2pkt.t -> unit
      (** Same as [xmit] method of {!Mac.t} class type. *)

    method private backend_reset_stats : unit
    method private backend_stats : 'stats
    method private backend_recv : L2pkt.t -> unit
end

(** The type of a MAC frontend. *)
class type virtual ['stats] frontend_t = 
object 
  method private frontend_reset_stats : unit
  method private frontend_stats : 'stats
  method private frontend_xmit : L2pkt.t -> unit
  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method bps : float

  method basic_stats : basic_stats
  method virtual private backend_recv : L2pkt.t -> unit
end

(**
  This is parameterized by the type ['stats] which is returned by the
  [#other_stats] method, for MAC layers which maintain more statistics than
  the {!Mac.basic_stats} stats. For those which do not, ['stats] should be of
  type [unit].*)
class type ['stats] stats_t  = 
object
  inherit t
  method other_stats : 'stats
end


(** The types of MAC that are available. *)
type mactype = 
  | Nullmac  (** See {!Mac_null.nullmac} *)
  | Contmac  (** See {!Mac_contention.contentionmac} *)
  | Cheatmac (** See {!Mac_cheat.cheatmac} *)
  | MACA_simple (** See {!MACA_simple.maca_mac} *)
  | MACA_contention (** See {!MACA_contention.maca_contentionmac} *)


val strset_mac : string -> unit
  (** Set the default mac via a string (for example provided as cmdline argument). *)

val mac : unit -> mactype
  (** Returns the mac type employed. *)


