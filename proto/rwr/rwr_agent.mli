(*
 * 
 * NAB - Network in a Box Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 * Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 * Laboratory for Computer Communications and Applications (LCA), Ecole
 * Polytechnique Federale de Lausanne (EPFL), CH-1015 Lausanne, Switzerland
 * 
 * This file is part of NAB. NAB is free software; you can redistribute it
 * and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 * 
 * NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details (enclosed in the file GPL).
 * 
 *)



val make_rwr_agent : ?stack:int -> #Node.node -> Rt_agent.t
  (** [make_rwr_agent ~stack n] creates a RWR routing
    agent attached to node [n] and stack [stack] (stack defaults to 0 if not
    provided).
  *)


module Rwr_stats : sig
  type stats = {
    mutable total_xmit : int; (** Total # packets transmitted. *)
    
    mutable data_xmit : int; (** Total # data packets transmitted. *)
    mutable data_orig : int; (** Total # data packets originated. *)
    mutable data_recv : int; (** Total # data packets received and delivered to
			       an upper layer (ie, data packets whose
			       destination was this node. *)
    
    mutable adv_xmit : int; (** Total # adv (beacon) packets transmitted *)
  }

  val add : stats -> stats -> stats 
    (** Return the sum of two a stats records. *)

  val null_stats : stats
    (** A [stats] object with all values set to 0 (ie, the state in
      which a new agent comes up. 
      Can be handy in tests, etc. *)
end


val agent_stats : ?stack:int -> Common.nodeid_t -> Rwr_stats.stats
  (** [agent_stats ~stack nid] returns the stats for node [nid] on stack [stack]
    (default stack is 0). *)

val reset_stats : ?stack:int -> Common.nodeid_t -> unit
(** [reset_stats ~stack nid] resets the stats for node [nid] on stack [stack]
    (default stack is 0). *)

val total_stats : ?stack:int -> unit -> Rwr_stats.stats
  (** Return the sum of statistics for all nodes running on a given stack
    (default stack is 0). *)



val run_until_convergence : ?stack:int -> unit -> unit
  (** Run the distributed bellmann-ford computation of the RWR metric until it
    converges. 
    NOTE: This function is a "cheat" in the sense that it takes a global
    shortcut to know when all agents have converged. *)
