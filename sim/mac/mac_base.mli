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
    (** Method documented in {!Mac.backend_t}. *)

  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
    (** To be implemented by the inheriting class (method documented in {!Mac.frontend_t}). *)
    
  method virtual xmit : L2pkt.t -> unit
    (** To be implemented by the inheriting class (method documented in {!Mac.frontend_t}). *)


  (** Methods documented in {!Mac.t}. *)
    
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
    (** To be implemented by the inheriting class (method documented in {!Mac.frontend_t}). *)


  method virtual private frontend_xmit : L2pkt.t -> unit
    (** To be implemented by the inheriting class (method documented in {!Mac.frontend_t}). *)

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
    (** To be implemented by the inheriting class (method documented in {!Mac.t}). *)

  method private unicast_failure : L2pkt.t -> unit
    (** Documented in {!Mac_base.base}. *)

  method virtual private backend_reset_stats : unit
    (** To be implemented by the inheriting class (method documented in {!Mac.backend_t}). *)

  method virtual private backend_stats : 'stats
    (** To be implemented by the inheriting class (method documented in {!Mac.backend_t}). *)

  method virtual private backend_xmit_complete : unit
    (** To be implemented by the inheriting class (method documented in {!Mac.backend_t}). *)

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
    (** To be implemented by the frontend class which is mixed in with this
      backend (method documented in {!Mac.frontend_t}). *)
end


type mac_queue_stats = 
    { nDrops : int } 
(** 
  A null backend with a packet queue. Like the plain null backend, this backend
  has no medium-access logic. However, it has an outgoing packet queue, for buffering
  packets when the frontend is busy.
    It models the following behavior:
    - nodes can only transmit one packet at the time. If this node is
    already transmitting, the new packet is sent to the queue.
    - nodes can receive at the same time from multiple neighbors
    - nodes can transmit and receive simultaneously
*)
class virtual queue_backend : ?stack:int -> ?buffer:int -> bps:float -> #Node.node ->
object
  inherit [mac_queue_stats] backend
  inherit [mac_queue_stats] Mac.backend_t

  method virtual private frontend_xmit : L2pkt.t -> unit
    (** To be implemented by the frontend class which is mixed in with this
      backend (method documented in {!Mac.frontend_t}). *)

  method virtual private state : Mac.frontend_state
    (** To be implemented by the frontend class which is mixed in with this
      backend (method documented in {!Mac.frontend_t}). *)
end


(** A Null frontend, which can be inherited from by MAC layers with no
  frontend logic, ie MAC layers which do no carrier sensing and do not model
  collisions. (for example see {!MACA_simple.maca_mac}). 

  All this one does is 
  - apply transmission delay.
  - provides a 'state' method to indicate current status of the frontend
  (idle, receiving, transmitting).
  - silently drops packets that it gets for transmission from the backend, if
  it is already transmitting or receiving.
  - does not receive packets if already receiving or transmitting.
  - provides a callback to the backend when a packet transmission is complete.
*)
class virtual null_frontend : ?stack:int -> bps:float -> #Node.node ->
object
  inherit [unit] frontend
  inherit [unit] Mac.frontend_t

  method virtual private backend_xmit_complete : unit
    (** To be implemented by backend class which is mixed in with this
      frontend (method documented in {!Mac.backend_t}). *)

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
