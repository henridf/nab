(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

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
      arrives at the node. So in order to be physically "truthful", a MAC
      implementation should wait an appropriate duration (depending on packet
      size and transmission rate of this MAC). *)
      
  method xmit : l2pkt:L2pkt.t -> unit

  method bps : float
end
    
type mactype = Nullmac | Contmac

val strset_mac : string -> unit
  (** Set the default mac via a string (for example provided as cmdline argument). *)

val mac : unit -> mactype
  (** Returns the mac type employed. *)


