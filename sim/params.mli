(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Globally accessible parameters.  *)

(* These parameters have not (yet?) been examined to figure out if they  *)
(* really need to be globally acessible.                                 *)

val nodes : int Param.t
  (** The number of nodes in the simulation. *)

val x_size : float Param.t
  (** The X [m] size  of the simulation area. *)

val y_size : float Param.t
  (** The Y [m] size  of the simulation area. *)

val rrange : float Param.t
  (** Radio range [m] of nodes *)

val ntargets : int Param.t
  (** The number of nodes that can potentially be routed to as destinations.
    In small simulations, this should be equal to the number of nodes. 
    For large simulations, some parts of mws may be more efficient when this
    is kept small. For example, in EASE routing, the size of the
    Last-Encounter table depends on this value *)
    
