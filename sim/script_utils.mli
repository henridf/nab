(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** General utils and helpers for writing MWS scripts *)


(** Arguments *)

val parse_args : unit -> unit

(** Setup/Initialization/Cleanup *) 

val init_sched : unit -> unit
  (** Instantiate the global scheduler object. Uses a Heap Scheduler by
    default. *)

val init_world : unit -> unit
  (** Instantiate the global world object. 
    Number of nodes (Params.nodes) should be set before calling this *)

val make_gpsnodes : unit -> unit
  (** Create gpsnodes, with no agents attached.
    Number of nodes (Params.nodes) should be set before calling this *)

val make_grep_nodes : unit -> unit 
  (** Create simplenodes each with a grep agent.
    Number of nodes (Params.nodes) should be set before calling this *)

val make_aodv_nodes : unit -> unit 
  (** Create simplenodes each with a aodv agent.
    Number of nodes (Params.nodes) should be set before calling this *)

val make_grease_nodes : unit -> unit 
  (** Create gpsnodes each with a grep agent.
    Number of nodes (Params.nodes) should be set before calling this *)




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
