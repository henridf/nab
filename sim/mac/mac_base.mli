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


(* 
   seems a bit weird that backend_recv *and* frontend_xmit should be both in
   frontend. 

   backend_recv in null_backend is currently lifted from null_mac, *except*
   that it doesn't compute delay (as done by null_mac#accept_).
   should this be done in null_frontend? 

   if it is done, we would probably be able to get a null mac by simply
   combining null frontend and null backend.

*)


(** Base classes for MAC layers. It is recommended to inherit from
  these classes when implementing a new MAC layer.

  The [base] class serves two purposes:
  - hide multistack details (see {!Node.node}
  for an explanation of multiple stacks) from the derived
  subclasses, to avoid having to keep track of which stack an agent is in
  (since in most cases nodes are run with only one stack).
  - specify virtual methods which must be implemented in order
  to conform to the {!Mac.t} interface.

  The [frontend] and [backend] classes also allow to break down a MAC
  implementation into a frontend radio component and a backend logic/protocol
  component. This way, different radio frontend implementations (ie a
  contention-detecting frontend, or a null frontend) can be mixed and matched
  with different backend implementations (ie a MACA backend or a null
  backend).

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
    
val xmit_time : float -> L2pkt.t -> Time.t
  (** [xmit_time bps l2pkt] returns the time necessary to transmit the packet
    [l2pkt] for an interface operating at [bps] bits per second. 
    This is transmission time only, ie propagation delay is not counted. *)

(** 
  The base MAC class. Inherit from this class for simple MAC layers which
  implement frontend and backend jointly. 

  This class is parameterized by the type ['stats] which is returned by the
  [#frontend_stats] method, for MACs which maintain more statistics than
  the {!Mac.basic_stats} stats. For those which do not, ['stats] should be of
  type [unit].

  @param stack serves to distinguish the stack when multiple stacks are being
  used. Defaults to 0. (The notion of multiple stacks is explained in
  {!Node.node}). 
  @param bps speed in bits per sec of this interface.
  @param node the node on which this MAC layer is running.
*)
class virtual ['stats] base : ?stack:int -> bps:float -> #Node.node ->
object
  inherit Log.inheritable_loggable 

  method private send_up : L2pkt.t -> unit
  method virtual other_stats : 'stats
    
  method private unicast_failure : L2pkt.t -> unit
    (** A MAC implementation may call this method on the base class when it
      a unicast transmission fails. If and when a MAC layer detects a unicast
      transmission failure depends on the type of MAC. For example, a MACAW
      mac would call [unicast_failure] when the RTS/CTS/DATA/ACK cycle
      fails. Or a MACA mac, which does not have ACKs, will never call
      [unicast_failure]. *)
    
  (** Methods documented in {!Mac.t}. *)
  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method virtual xmit : L2pkt.t -> unit
  method basic_stats : Mac.basic_stats
  method reset_stats : unit
  method bps : float
end

(**
  The base frontend MAC class. 
  Inherit from this class when implementing a MAC frontend (see for example
  {!Contention_frontend.contention_frontend}).

  This class is parameterized by the type ['stats] which is returned by the
  [#frontend_stats] method, for frontends which maintain more statistics than
  the {!Mac.basic_stats} stats. For those which do not, ['stats] should be of
  type [unit].

  @param stack serves to distinguish the stack when multiple stacks are being
  used. Defaults to 0. (The notion of multiple stacks is explained in
  {!Node.node}). 
  @param bps speed in bits per sec of this interface.
  @param node the node on which this MAC layer is running.
*)
class virtual ['stats] frontend : ?stack:int -> bps:float -> #Node.node ->
object
  inherit Log.virtual_loggable 

  (** These counters are used to track {!Mac.basic_stats}. *)
  val mutable bitsTX : int
  val mutable bitsRX : int
  val mutable pktsTX : int
  val mutable pktsRX : int

  method bps : float

  method basic_stats : Mac.basic_stats
    (** Returns a {!Mac.basic_stats} record. *)

  method virtual private frontend_stats : 'stats
    (** Returns a ['stats] record, for those frontends which maintain
      additional statistics. *)

  method virtual private frontend_reset_stats : unit
    (** Reset all internal statistics counters (both those for
      {!Mac.basic_stats} and any additional stats. *)

  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit

  method virtual private frontend_xmit : L2pkt.t -> unit
    (** This method is called on the frontend by the backed with a packet
      which is to be sent straight out over the air. *)

  method virtual private backend_recv : L2pkt.t -> unit
    (** The frontend should call this method with an incoming packet which has
      been read off the radio, in order to hand it off to the backend.
      This should not be implemented in the inheriting frontend mac, but
      rather in the backend which is then 'mixed in' with this frontend. *)
end



(**
  The base backend MAC class. 
  Inherit from this class when implementing a MAC backend (see for example
  {!MACA_backend.maca_backend}).

  This class is parameterized by the type ['stats] which is returned by the
  [#backend_stats] method, for backends which maintain any statistics. For
  those which do not, ['stats] should be of type [unit].

  @param stack serves to distinguish the stack when multiple stacks are being
  used. Defaults to 0. (The notion of multiple stacks is explained in
  {!Node.node}). 
  @param bps speed in bits per sec of this interface.
  @param node the node on which this MAC layer is running.
*)
class virtual ['stats] backend : ?stack:int -> bps:float -> #Node.node ->
object
  inherit Log.virtual_loggable 

  method private send_up : L2pkt.t -> unit

  method virtual xmit : L2pkt.t -> unit
    (** Documented in {!Mac.t}. *)

  method private unicast_failure : L2pkt.t -> unit
    (** Documented in {!Mac_base.base}. *)

  method virtual private backend_reset_stats : unit
  method virtual private backend_stats : 'stats
end


(** 
  A Null backend, which can be inherited from by MAC layers with no backed
  logic (for example see {!Mac_contention.contentionmac}). 
  Packets from the upper layers are sent directly to the frontend, and
  vice-versa. It maintains no statistics.
*)
class virtual null_backend : ?stack:int -> bps:float -> #Node.node ->
object
  inherit [unit] backend
  method private backend_reset_stats : unit
  method private backend_stats : unit
  method virtual private frontend_xmit : L2pkt.t -> unit
end


(** A Null frontend, which can be inherited from by MAC layers with no backend
  logic (for example see {!MACA_simple.maca_mac}). *)
class virtual null_frontend : ?stack:int -> bps:float -> #Node.node ->
object
  inherit [unit] frontend
  method private frontend_reset_stats : unit
  method private frontend_stats : unit
  method private frontend_xmit : L2pkt.t -> unit
  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
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
