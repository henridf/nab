(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



(* Return node by index *)
val node : int -> Node.node_t
  
val iter : (Node.node_t -> unit) -> unit
val map : (Node.node_t -> 'a) -> 'a array
val fold : (Node.node_t -> 'a -> 'a) -> 'a -> 'a

val set_nodes : Node.node_t array -> unit

