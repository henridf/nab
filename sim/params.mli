(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* Globally accessible parameters.                                       *)
(* These parameters have not (yet?) been examined to figure out if they  *)
(* really need to be globally acessible.                                 *)

val warmup : bool Param.t
val warmup_percent : float Param.t
val nodes : int Param.t
val x_size : float Param.t
val y_size : float Param.t
val rrange : float Param.t
val ntargets : int Param.t
val nodes_file : string Param.t
val trace_enabled : bool Param.t
