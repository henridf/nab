(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(** 
  Ether: The shared medium onto which nodes transmit. 
  @author Henri Dubois-Ferriere 
*)

val speed_of_light : float
val propdelay : Coord.coordf_t -> Coord.coordf_t -> float
  (** Computes the propagation delay (at light speed) between 
    two coordinates (expressed in meters). *)

(*
val bits_per_sec : float
val xmitdelay : bytes:int -> float
  (** Computes the transmission delay for [bytes] bytes, assuming transmission
    speed of [bits_per_sec]. Note: bits_per_sec is a MAC/PHY-dependant property, not a
    fixed property of the ether - so this should move. *)
*)

module type Ether_t = sig 
  val emit : ?stack:int -> nid:Common.nodeid_t -> L2pkt.t -> unit 
  (** A node's MAC calls this to emit bits into the air. The Ether module
    then takes care of sending them, with appropriate propagation delay and SNR,
    to nodes within range. *)
end

module SimpleEther : Ether_t
