




(** 
  Ether: The shared medium onto which nodes transmit. 
  @author Henri Dubois-Ferriere 
*)

val speed_of_light : float
val propdelay : Coord.coordf_t -> Coord.coordf_t -> float
  (** Computes the propagation delay (at light speed) between 
    two coordinates (expressed in meters). *)


module type Ether_t = sig 
  val emit : ?stack:int -> nid:Common.nodeid_t -> L2pkt.t -> unit 
  (** A node's MAC calls this to emit bits into the air. The Ether module
    then takes care of sending them, with appropriate propagation delay and SNR,
    to nodes within range. 
    [stack] serves to distinguish when multiple stacks are being used. 
    The notion of multiple stacks is explained in {!Simplenode.simplenode}. *)
end

module SimpleEther : Ether_t
