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

(* $Header$ *)



(** Persistency-related functions for saving and 
  restoring simulation state.

  @author Henri Dubois-Ferriere.
 *)


val save_node_state : 
  ?gpsnodes:bool -> 
  out_channel -> 
  unit
  (** Save node-specific state. Right now this state consists of node
    positions only. (Might at a later point contain a spec describing mobility
    process, traffic sources attached to the node, agent(s), mac layer, etc).
  *)

val read_node_state : ?gpsnodes:bool -> 
  in_channel -> 
  unit
  (** Restore node-specific state from in_channel. 
     Note that world object should be initialized with the appropriate # of
     nodes before calling this (which is somewhat counterproductive, agreed..)
  *)

val save_grep_agents : ?stack:int -> out_channel -> unit
  (** Save state of all {!Grep_agent.grep_agent} objects. *)

val read_grep_agents : ?stack:int -> in_channel -> unit
  (** Creates new {!Grep_agent.grep_agent} objects initialized with the state
    read in from the provided channel. Note that any existing grep_agents are
    lost.
  *)
