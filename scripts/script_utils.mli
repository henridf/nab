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



(** General utils and helpers for writing nab scripts and apps.
  @author Henri Dubois-Ferriere. 
*)


(** Arguments *)

val parse_args : unit -> unit
val print_header : unit -> unit

(** Setup/Initialization/Cleanup *) 

val init_sched : unit -> unit
  (** Instantiate the global scheduler object. Uses a Heap Scheduler by
    default. *)

val init_lazy_world : unit -> unit
  (** Instantiate a {!Crworld.world_lazy} global world object with
    reflecting boundaries.
    Number of nodes {!Params.nodes} and radio range {!Params.radiorange} should be
    set before calling this. *)


val init_lazy_taurus_world : unit -> unit
  (** Instantiate a {!Crworld.world_lazy} global world object with wrapping
    boundaries (taurus topology).
    Number of nodes {!Params.nodes} and radio range {!Params.radiorange} should be
    set before calling this. *)


val init_greedy_world : unit -> unit
  (** Instantiate a {!Crworld.world_greedy} global world object with
    reflecting boundaries.
    Number of nodes {!Params.nodes} and radio range {!Params.radiorange} should be
    set before calling this. *)

  
val init_greedy_taurus_world : unit -> unit
  (** Instantiate a {!Crworld.world_greedy} global world object with
    wrapping boundaries (taurus topology).
    Number of nodes {!Params.nodes} and radio range {!Params.radiorange} should be
    set before calling this. *)
  

val init_epfl_world : unit -> unit
  (** Instantiate a {!Crworld.epflworld} global world object.
    Number of nodes {!Params.nodes} and radio range {!Params.radiorange} should be
    set before calling this. *)


val init_all : unit -> unit
  (** Instantiates both the global world object (a lazy one) and the global
    scheduler object, and sets the time to 0.0 *)

val size : ?rrange:float -> ?nodes:int -> avg_degree:int -> unit -> float
  (** Returns the side of a square surface to get the required average node
    degree, given the number of nodes and radio range *)
    
val make_nodes : ?pos_aware:bool -> ?with_positions:bool -> unit -> unit
  (** Create {!Simplenode.simplenode} each with a mac layer of the type
    specified in {!Params.mac}.

    Number of nodes {!Params.nodes} should be set before calling this.

    Optional [with_positions] (true by default) determines whether to initialize nodes with
    (randomly generated) positions. It can be passed as [false]
    for example if node positions are being restored from a prior run.
    Optional [pos_aware] (false by default) determines whether the nodes are
    position aware.
 *)

val make_naked_nodes : ?pos_aware:bool -> ?with_positions:bool -> unit -> unit
  (** Create {!Simplenode.simplenode} nodes with no routing agents or MAC layers.

    Number of nodes {!Params.nodes} should be set before calling this.
    Optional [with_positions] (true by default) determines whether to initialize nodes with
    (randomly generated) positions. It can be passed as [false]
    for example if node positions are being restored from a prior run.
    Optional [pos_aware] (false by default) determines whether the nodes are
    position aware.
 *)

val make_grep_nodes : unit -> unit 
  (** Create {!Simplenode.simplenode} each with a grep agent and a mac layer
    of the type specified in {!Params.mac}.
    Number of nodes {!Params.nodes} should be set before calling this *)


val make_flood_agents : ?stack:int -> unit -> unit 
  (** Creates and adds a simple flooding agent to each node, on [stack]
    (default stack 0). See {!Flood_agent.flood_agent}.
    Nodes should be created before calling this.*)
  
val make_ler_agents : ?stack:int -> Ease_agent.ler_proto_t -> unit 
  (** Creates and adds a LER agent to each node, on [stack]
    (default stack 0). 
    The parameter {!Ease_agent.ler_proto_t} indicates the choice of algorithm 
    that the node should run (FRESH, EASE, GREASE).

    Nodes should be created before calling this. EASE and GREASE require
    position aware nodes (see [make_nodes] and [make_naked_nodes] above).
*)

val install_macs : ?stack:int ->  ?bps:float -> unit -> unit
  (** Installs a Mac layer of type {!Params.mac} on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Simplenode.simplenode}. *)

val install_null_macs : ?stack:int ->  ?bps:float -> unit -> unit
  (** Installs a Nullmac Mac layer on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Simplenode.simplenode}. *)

val install_cheat_macs : ?stack:int ->  ?bps:float -> unit -> unit
  (** Installs a Cheatmac layer on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Simplenode.simplenode}. *)

val install_contention_macs : ?stack:int -> ?bps:float -> unit -> unit
  (** Installs a Contentionmac Mac layer on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Simplenode.simplenode}. *)


val place_nodes_on_line : unit -> unit
  (** Places all nodes on a horizontal line, evenly spaced so as to use 
    the whole width {!Params.x_size}*)

(** Stats *)

val avg_neighbors_per_node : unit -> float 



(* Graphics *)
(*
val draw_nodes : unit -> unit
val draw_node : nid:Common.nodeid_t -> unit
val label_node : node:Node.node_t -> unit
val label_nodes : unit -> unit
val redraw_and_label_nodes : unit -> unit  
val wait_for_any_keypress : unit -> unit



val gui_grep_one_route : unit -> unit
val gui_draw_connectivity : unit -> unit

*)

val detach_daemon :  outfilename:string -> unit
  (** Detach from terminal. All further logs will be spewed to outfilename *)


val interactive_print_banner : string -> unit


