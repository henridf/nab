(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Traffic Generators. 
  @author Henri Dubois-Ferriere.
*)

type traffic_generator_t = (unit -> float option)
    (** A traffic generator is a function that either returns 
      - Some t, where t is the interval till the next packet to send.
      - None, if there are no more packets to send*)


val make_cbr : num_pkts:int -> pkts_per_sec:float -> traffic_generator_t
  (** Returns a traffic_generator_t which will originate num_pkts, at a rate
    of pkts_per_sec *)

val make_poisson :  num_pkts:int -> lambda:float -> traffic_generator_t
  (** Returns a traffic_generator_t which will originate num_pkts, spaced
    with an exponential distribution of parameter lambda (mean 1/lambda) *)
