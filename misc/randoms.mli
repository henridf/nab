(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Wrappers around the standard library Random module, to de-multiplex
  multiple separate RNG streams out of a single source.
  @author Henri Dubois-Ferriere.
*)

val seed : int ref
  (** Value last used to seed the RNG (with Random.init) *)

val change_seed : unit -> unit
  (** Change the RNG seed. *)

val init_seed : unit -> unit
  (** Change the RNG seed back to the initial value (the one that this starts
    off with). This would be useful for example when we have run a bunch of
    GREP tests and now want to start all over again and run a bunch of AODV tests.*)



type handle

val create : ?seed:int -> unit -> handle

val int : handle -> int -> int

val bool : handle -> bool

val float : handle -> float -> float

