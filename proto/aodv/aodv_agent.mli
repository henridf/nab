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


(** A simple implementation of the AODV routing protocol.

  This implementation is not yet a literal and exhaustive transcription of the
  RFC, in particular the following functions have yet to be implemented:
  - Features for dealing with unidirectional links (blacklists, 'A' option to
  request a RREP-ACK) are not supported.
  - Actions after reboot.
  - Section 6.10 of the RFC ("Maintaing Local Connectivity") is partially
  implemented.
*)


(** Statistics record maintained by aodv_agent. *)
module Aodv_stats : sig
  type stats = {
    mutable total_xmit : int; (** Total # packets transmitted. *)
    
    mutable data_xmit : int; (** Total # data packets transmitted. *)
    mutable data_orig : int; (** Total # data packets originated. *)
    mutable data_recv : int; (** Total # data packets received and delivered to
			       an upper layer (ie, data packets whose
			       destination was this node. *)
    
    mutable hello_xmit : int; (** Total # hello packets transmitted. *)
    
    mutable rreq_xmit : int; (** Total # rreq packets transmitted *)
    mutable rreq_init : int; (** Total # rreqs initiated. This is incremented
			       once at the start of each route request cycle,
			       not on subsequent ERS searches within that route
			       request cycle.   *)

    mutable rreq_orig : int; (** Total # rreq packets originated. This is
			       incremented at each new rreq packet originated
			       (including for subsequent retries when using
			       ERS). *)
    
    mutable rerr_xmit : int; (** Total # rerr packets transmitted. *)
    mutable rerr_orig : int; (** Total # rerr packets originated. *)

    mutable rrep_xmit : int; (** Total # rrep packets transmitted. *)
    mutable rrep_orig : int; (** Total # rrep packets originated. *)
    mutable rrep_drop_nohop : int; (** Total # rrep packets dropped because no
				     route to originator. *)
    mutable data_drop_overflow : int; (** Total # data packets dropped due to
					buffer overflow. *)
    mutable data_drop_rerr : int; (** Total # data packets dropped due to route
				    error. *)
  }

  val add : stats -> stats -> stats 
    (** Return the sum of two a stats records. *)

  val sprint_stats : stats -> string
    (** Print a stats record into a nice string. *)

  val null_stats : stats
    (** A [stats] object with all values set to 0 (ie, the state in
      which a new agent comes up. 
      Can be handy in tests, etc. *)
    
end

val total_stats : ?stack:int -> unit -> Aodv_stats.stats
  (** Return the sum of statistics for all nodes running on a given stack
    (default stack is 0). *)
  

type persist_t = {
  localrepair:bool;
  dstonly:bool;
  seqno:int;
  stats:Aodv_stats.stats;
  rt:Aodv_rtab.t
}

class aodv_agent : ?stack:int -> ?localrepair:bool -> ?dstonly:bool -> #Node.node -> 
object

  inherit Rt_agent.t 

  method rtab : unit -> Aodv_rtab.t
    (** Routing table is only exposed so that tests can look at it. *)

  method stats : Aodv_stats.stats
    (** Return [stats] object for this AODV agent. *)

  method read_state : persist_t -> unit
    (** Support for persistency (should not be used directly). *)

  method dump_state : unit -> persist_t
    (** Support for persistency (should not be used directly). *)

end

module Persist : Persist.t 
  (** Aodv agents can be saved and restored as per the {!Persist.t}
    interface. *)
