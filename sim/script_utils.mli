(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(* Setup/Initialization/Cleanup *) 
val init_sched : unit -> unit
val init_world : unit -> unit

val make_grep_nodes : unit -> unit (* consults Params.nodes *)
val make_grease_nodes : unit -> unit (* consults Params.nodes *)
val make_aodv_nodes : unit -> unit (* consults Params.nodes *)

val cleanup : unit -> unit

(* Actions *)
val move_nodes : 
  prop:float -> (* btw 0 and 1 *)
  targets:int -> 
  unit

(* Stats *)
val proportion_met_nodes : targets:int -> float 
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
