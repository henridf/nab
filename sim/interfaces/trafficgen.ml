



(** Traffic Generator types. 
  @author Henri Dubois-Ferriere.
*)

type t = (unit -> float option)
    (** A traffic generator is a function that either returns 
      - Some t, where t is the interval till the next packet to send.
      - None, if there are no more packets to send*)

