(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Wrappers around the standard library Random module, to allow 'rewind' in
  the RNG stream and de-multiplexing multiple separate RNG streams out of a
  single source.

  The first feature ('rewind') is useful for example if we have a simulation
  script where we wish to re-run the same scenario (with identical mobility,
  traffic pattern) many times, with a different protocol set up at each time. 
  With this we can set the seed back to its starting value and be sure that we
  are testing each protocol over exactly the same inputs.

  The second feature ('demultiplex') is useful for example so that introducing a
  new component to an existing simulation script does not 'perturb' the
  random number sequence when this component requires random numbers. 

  @author Henri Dubois-Ferriere.
*)

(** {2 Global RNG} *)

val change_seed : ?newseed:int -> unit -> unit
  (** Change the RNG seed. *)

val rewind_seed : unit -> unit
  (** Bring the RNG seed back to the value it was given in the last call to
    {!Randoms.rewind_seed}, or it's initial value if {!Randoms.rewind_seed} has never been called. *)



(** {2 RNG Streams} *)

type rng_stream
  (** A type used as a handle for an individual RN stream *)

val create : ?seed:int -> unit -> rng_stream
  (** Create a stream. Starting seed is optional; if not provided an
    unspecified seed is chosen *) 

val int : rng_stream -> int -> int
  (** Returns a random integer from the given stream. See Random.int from the
    standard library. *)

val bool : rng_stream -> bool
  (** Returns a random boolean from the given stream. See Random.bool from the
    standard library. *)

val float : rng_stream -> float -> float
  (** Returns a random float from the given stream. See Random.float from the
    standard library. *)

