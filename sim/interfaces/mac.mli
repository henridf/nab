



(** 
  MAC layer interface and helper functions.

  @author Henri Dubois-Ferriere.
*)


(**  The interface which a MAC layer must implement.

  Note for those implementing a MAC layer: it is simplest to inherit
  from {!Mac_base.base}, for the reasons described therein.
  For a simple example of a MAC class, see {!Mac_null.nullmac}.
*)
class type t  = 
object

  inherit Log.inheritable_loggable 

  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
    (** [recv] is called when a packet arrives at a node.

      More precisely, [recv] is called when the {i first bit} of a packet
      arrives at the node. So, a realistic MAC implementation should wait an
      appropriate duration (depending on packet size and transmission rate of
      this MAC). A MAC that detects collisions would also keep track of the
      fact that it is receiving for this duration, in order to know that there
      is a collision if another packet is received in this interval.
    *)
      
  method xmit : l2pkt:L2pkt.t -> unit

  method bps : float
end
    
(** The types of MAC that are available. *)
type mactype = 
  | Nullmac  (** See {!Mac_null.nullmac} *)
  | Contmac  (** See {!Mac_contention.contentionmac} *)
  | Cheatmac (** See {!Mac_cheat.cheatmac} *)

val strset_mac : string -> unit
  (** Set the default mac via a string (for example provided as cmdline argument). *)

val mac : unit -> mactype
  (** Returns the mac type employed. *)


