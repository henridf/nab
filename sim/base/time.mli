



(** Handling of time.
  @author Henri Dubois-Ferriere.
*)

type time_t = float

val set_time : time_t -> unit
  (** Set the current time. This should only be used for good reason, for
    example if loading agents with pre-generated Last-Encounter tables, when
    we need to set the clocks to the time at which the warmup ended. *)

val get_time : unit -> time_t
  (** Returns the present simulator time. *)

val time: unit -> time_t
  (** Identical to [get_time]. *)

