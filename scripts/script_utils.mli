




(** General utils and helpers for writing MWS scripts.
  @author Henri Dubois-Ferriere. 
*)


(** Arguments *)

val parse_args : unit -> unit

(** Setup/Initialization/Cleanup *) 

val init_sched : unit -> unit
  (** Instantiate the global scheduler object. Uses a Heap Scheduler by
    default. *)

val init_lazy_world : unit -> unit
  (** Instantiate a {!Crworld.crworld_lazy} global world object with
    reflecting boundaries.
    Number of nodes {!Params.nodes} should be set before calling this *)

val init_lazy_taurus_world : unit -> unit
  (** Instantiate a {!Crworld.crworld_lazy} global world object with wrapping
    boundaries (taurus topology).
    Number of nodes {!Params.nodes} should be set before calling this *)

val init_greedy_world : unit -> unit
  (** Instantiate a {!Crworld.crworld_greedy} global world object with
    reflecting boundaries.
    Number of nodes {!Params.nodes} should be set before calling this *)
  
val init_greedy_taurus_world : unit -> unit
  (** Instantiate a {!Crworld.crworld_greedy} global world object with
    wrapping boundaries (taurus topology).
    Number of nodes {!Params.nodes} should be set before calling this *)

val init_epfl_world : unit -> unit
  (** Instantiate a {!Crworld.epflworld} global world object.
    Number of nodes {!Params.nodes} should be set before calling this *)

val init_all : unit -> unit
  (** Instantiates both the global world object (a lazy one) and the global scheduler
    object, and sets the time to 0.0 *)

val size : ?rrange:float -> ?nodes:int -> avg_degree:int -> unit -> float
  (** Returns the side of a square surface to get the required average node
    degree, given the number of nodes and radio range *)
    
val make_nodes : ?with_positions:bool -> unit -> unit
  (** Create {!Simplenode.simplenode} each with a mac layer of the type
    specified in {!Params.mac}.

    Number of nodes {!Params.nodes} should be set before calling this.

    Optional [with_positions] determines whether to initialize nodes with
    (randomly generated) positions. It is true by default, and should only be
    false if node positions are being restored from a prior run.

 *)

val make_grep_nodes : unit -> unit 
  (** Create {!Simplenode.simplenode} each with a grep agent and a mac layer
    of the type specified in {!Params.mac}.
    Number of nodes {!Params.nodes} should be set before calling this *)

val make_diff_agents : unit -> unit 
  (** Adds a diffusion agent to each node.
    Nodes should be created before calling this.*)

val make_aodv_nodes : unit -> unit 
  (** Create {!Simplenode.simplenode} each with a aodv agent and a mac layer
    of the type specified in {!Params.mac}.
    Number of nodes {!Params.nodes} should be set before calling this *)

val make_grease_nodes : unit -> unit 
  (** Create gpsnodes each with a EASE agent and a mac layer
    of the type specified in {!Params.mac}.
    Number of nodes {!Params.nodes} should be set before calling this *)

val place_nodes_on_line : unit -> unit
  (** Places all nodes on a horizontal line, evenly spaced so as to use 
    the whole width {!Params.x_size}*)

val install_macs : ?stack:int -> unit -> unit
  (** Installs a Mac layer of type {!Params.mac} on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Simplenode.simplenode}. *)

val install_null_macs : ?stack:int -> unit -> unit
  (** Installs a Nullmac Mac layer on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Simplenode.simplenode}. *)

val install_contention_macs : ?stack:int -> unit -> unit
  (** Installs a Contentionmac Mac layer on each node.
    Nodes should be created before calling this.
    Optional [stack] is explained in {!Simplenode.simplenode}. *)



(** Actions *)

val move_nodes : 
  prop:float -> (* btw 0 and 1 *)
  unit

(** Stats *)

val avg_neighbors_per_node : unit -> float 

val grep_one_route : src:Common.nodeid_t -> dst:Common.nodeid_t -> unit

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




