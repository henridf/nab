(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Globally accessible parameters.                                       *)
(* These parameters have not (yet?) been examined to figure out if they  *)
(* really need to be globally acessible.                                 *)

val warmup : bool Param.t
val warmup_percent : float Param.t
val minrange : float Param.t
val maxrange : float Param.t
val nodes : int Param.t
val ntargets : int Param.t
val algo : Common.route_algorithm Param.t
val mob : Common.mobility_pattern Param.t
val top : Common.topology Param.t
val nodes_file : string Param.t
val action : Common.action Param.t
