(** Base class for MAC layers. 
  This class essentially serves to hide multistack details (see {!Simplenode.simplenode}
  for an explanation of multiple stacks) from the derived
  subclasses, to avoid having to keep track of which stack an agent is in
  (since in most cases nodes are run with only one stack).

  The class also specifies virtual methods which must be implemented in order
  to conform to the {!Mac.t} interface.

  When implementing a new MAC layer, it is recommended to inherit from
  this class.



  @author Henri Dubois-Ferriere.
*)

class virtual base : ?stack:int -> bps:float -> #Simplenode.simplenode ->
  (** [stack] serves to distinguish when multiple stacks are being used. 
    The notion of multiple stacks is explained in {!Simplenode.simplenode}. *)
object
  val myid : Common.nodeid_t
  method private xmitdelay : bytes:int -> float
  method private send_up : l2pkt:L2pkt.t -> unit

  method virtual recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method virtual xmit : l2pkt:L2pkt.t -> unit

end
