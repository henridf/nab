



(** Wrappers around the standard library [Random] module, to allow rewinding
  of the global RNG stream.

  This is useful for example if we have a simulation
  script where we wish to re-run the same scenario (with identical mobility,
  traffic pattern) many times, with a different protocol set up at each time. 
  With this we can set the seed back to its starting value and be sure that we
  are testing each protocol over exactly the same inputs.

  @author Henri Dubois-Ferriere.
*)


val change_seed : ?newseed:int -> unit -> unit
  (** Change the RNG seed. *)

val rewind_seed : unit -> unit
  (** Bring the RNG seed back to the value it was given in the last call to
    {!Randoms.rewind_seed}, or it's initial value if {!Randoms.rewind_seed} has never been called. *)



