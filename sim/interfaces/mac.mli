(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Header *)







(** 
  MAC layer interface and helper functions.

  @author Henri Dubois-Ferriere.
*)

type basic_stats = 
    { bits_RX : int;
      bits_TX : int}
    (** The stats every MAC layer *must* maintain. As basic as it gets.
      Of course, most MACs will want to maintain other stats as well - this is
      not specified in the common MAC interface, since the specific stats will
      vary depending on the characteristics of the MAC. For an example, see
      [method other_stats] in mac_base.mli. *)



(**  The interface which a MAC layer must implement.

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
      
  method xmit : l2pkt:L2pkt.t -> unit
    (** [xmit] is called by the upper layers on the node containing this MAC
      to send a packet out. *)

  method bps : float
    (** Return this MAC's bps speed. *)
  method basic_stats : basic_stats
    (** Return stats for this MAC object. *)

  method reset_stats : unit
    (** Reset stats for this MAC object. *)
end
    
(** The types of MAC that are available. *)
type mactype = 
  | Nullmac  (** See {!Mac_null.nullmac} *)
  | Contmac  (** See {!Mac_contention.contentionmac} *)
  | Cheatmac (** See {!Mac_cheat.cheatmac} *)

val strset_mac : string -> unit
  (** Set the default mac via a string (for example provided as cmdline argument). *)

val mac : unit -> mactype
  (** Returns the mac type employed. *)


