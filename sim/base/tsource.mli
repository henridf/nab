



(** Traffic Generators. 
  @author Henri Dubois-Ferriere.
*)

val make_cbr : ?num_pkts:int -> pkts_per_sec:float -> unit -> Trafficgen.t
  (** Returns a traffic generator which originates packets, at a fixed rate
    of [pkts_per_sec]. If [num_pkts] is provided, traffic generator will stop
    after [num_pkts], otherwise generates packets indefinitely. *)

val make_poisson :  ?num_pkts:int -> lambda:float -> unit -> Trafficgen.t 
  (** Returns a traffic generator which will originate num_pkts, spaced
    with an exponential distribution of parameter lambda (mean 1/lambda).
    If [num_pkts] is provided, traffic generator will stop after [num_pkts],
    otherwise generates packets indefinitely.
  *)
