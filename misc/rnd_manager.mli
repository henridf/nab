(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Wrappers around the standard library Random module.
  This doesn't do anything smart - just provide a standard way of keeping
  track of the seed, changing it, and returning to a previous value of the
  seed. At some point, this might allow to de-multiplex separate RNG streams
  out of the single RNG source to allow for example some new module to pull
  random numbers without changing the randoms that the mobility generation
  module gets.
*)

val seed : int ref
  (** Value last used to seed the RNG (with Random.init) *)

val change_seed : unit -> unit
  (** Change the RNG seed. *)

val init_seed : unit -> unit
  (** Change the RNG seed back to the initial value (the one that this starts
    off with). This would be useful for example when we have run a bunch of
    GREP tests and now want to start all over again and run a bunch of AODV tests.*)
