




(** Note that there can only be one world object *)

val set_lazy_world : Worldt.lazy_world_t -> unit
  (** Sets the global lazy world object. *)

val set_greedy_world : Worldt.greedy_world_t -> unit
  (** Sets the global lazy world object. *)

val w : unit -> Worldt.lazy_world_t
  (** Returns the global lazy_world object, if one has been set. 
    Otherwise, returns the global greedy_world object, coerced to a
    lazy_world, if a greedy_world has been set. *)

val gw : unit -> Worldt.greedy_world_t
  (** Returns the global greedy_world object, if one has been set. *)
