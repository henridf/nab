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

val parse_args : 
  ?extra_argspec:(Arg.key * Arg.spec * Arg.doc) list -> 
  ?anon_fun:Arg.anon_fun -> unit -> unit
  (** Parse arguments, according to all created Param ({!Param.t} objects
    which are cmdline-settable.
    
    @extra_spec Additional command-line options to accept, empty by default.
    See the Arg module of the ocaml standard lib for further documentation.
    @anon_fun Function to call on anonymous arguments, none by default.
    See the Arg module of the ocaml standard lib for further documentation.
  *)

val print_header : unit -> unit

(** Setup/Initialization/Cleanup *) 

val init_world : unit -> unit
  (** Instantiates the world object of type indicated by the world param
    {!World.world}. 
    Number of nodes ({!Params.nodes}), world size ({!Params.x_size} and
    {!Params.y_size}), and radio range ({!Params.radiorange}) should be
    set before calling this. *)

val init_all : unit -> unit
  (** Instantiates the global world object, the global
    scheduler object, and sets the time to 0.0 *)

val size : ?rrange:float -> ?nodes:int -> avg_degree:int -> unit -> float
  (** Returns the side of a square surface to get the required average node
    degree, given the number of nodes and radio range *)
    
val make_nodes :  ?with_positions:bool -> unit -> unit
  (** Create {!Node.node} each with a mac layer of the type
    specified in {!Params.mac}.

    Number of nodes {!Params.nodes} should be set before calling this.

    Optional [with_positions] (true by default) determines whether to initialize nodes with
    (randomly generated) positions. It can be passed as [false]
    for example if node positions are being restored from a prior run.
 *)

val make_naked_nodes : ?with_positions:bool -> unit -> unit
  (** Create {!Node.node} nodes with no routing agents or MAC layers.

    Number of nodes {!Params.nodes} should be set before calling this.
    Optional [with_positions] (true by default) determines whether to initialize nodes with
    (randomly generated) positions. It can be passed as [false]
    for example if node positions are being restored from a prior run.
 *)

val make_aodv_nodes : ?localrepair:bool -> ?dstonly:bool -> unit -> unit 
  (** Create {!Node.node} each with a aodv agent and a mac layer
    of the type specified in {!Params.mac}.
    @param localrepair corresponds to the local repair operation as defined in
    the RFC, and is true by default .
    @param dstonly corresponds to operation with the 'destination only' flag
    set, as defined in the RFC, and is false by default.

    Number of nodes {!Params.nodes} should be set before calling this *)

val make_str_nodes : Str_rtab.metric_t -> unit 
  (** Create {!Node.node} each with a STR agent and a mac layer
    of the type specified in {!Params.mac}.
    Number of nodes {!Params.nodes} should be set before calling this *)


val make_flood_agents : ?stack:int -> unit -> unit 
  (** Creates and adds a simple flooding agent to each node, on [stack]
    (default stack 0). See {!Flood_agent.flood_agent}.
    Nodes should be created before calling this.*)
  
val make_ler_agents : ?stack:int -> Ler_agent.ler_proto_t -> unit 
  (** Creates and adds a LER agent to each node, on [stack]
    (default stack 0). 
    The parameter {!Ler_agent.ler_proto_t} indicates the choice of algorithm 
    that the node should run (FRESH, EASE, GREASE).

    Nodes should be created before calling this. EASE and GREASE require
    position aware nodes (see [make_nodes] and [make_naked_nodes] above).
*)

val install_macs : ?stack:int ->  ?bps:float -> unit -> unit
  (** Installs a Mac layer of type {!Params.mac} on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Node.node}. *)

val install_null_macs : ?stack:int ->  ?bps:float -> unit -> unit
  (** Installs a Nullmac Mac layer (see {!Mac_null.nullmac}) on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Node.node}. *)

val install_queue_null_macs : ?stack:int ->  ?bps:float -> unit -> unit
  (** Installs a Null Mac with sendqueue (see {!Mac_null_queue.nullmac_q}) on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Node.node}. *)

val install_cheat_macs : ?stack:int ->  ?bps:float -> unit -> unit
  (** Installs a Cheatmac layer on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Node.node}. *)

val install_contention_macs : ?stack:int -> ?bps:float -> unit -> unit
  (** Installs a Contentionmac Mac layer on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Node.node}. *)

val install_mobs : ?gran:float -> unit -> unit

val place_nodes_on_line : unit -> unit
  (** Places all nodes on a horizontal line, evenly spaced so as to use 
    the whole width {!Params.x_size}*)

(** Stats *)

val avg_neighbors_per_node : unit -> float 


val detach_daemon :  ?outfilename:string -> unit -> unit
  (** Detach from terminal. All further logs will be spewed to outfilename
    (if not provided, logs go to a file in the current directory named like
    nab-2004-07-02-14h50m29.log).
 *)


val interactive_print_banner : string -> unit


(**/**)
val init_sched : unit -> unit
  (** Instantiate the global scheduler object. Uses a Heap Scheduler by
    default. *)

