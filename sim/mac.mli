(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  MAC layer class type and helper functions.
  @author Henri Dubois-Ferriere.
*)


(** The interface that a MAC class should implement.
  A MAC class should also inherit from {!Mac_base.base}.

  For a simple example of a MAC class, see {!Mac.mac_null}.
*)
class type mac_t  = 
object

  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
    (** This is called when a packet arrives at a node.

      More precisely, [recv] is called when the {i first bit} of a packet
      arrives at the node. So in order to be physically "truthful", a MAC
      implementation should wait an appropriate duration (depending on packet
      size and transmission rate of this MAC). *)
      
  method xmit : l2pkt:L2pkt.t -> unit

  (**/**)
  method objdescr : string
end
    
type mactype = Nullmac | Contmac

val strset_mac : string -> unit
  (** Set the default mac via a string (for example provided as cmdline argument). *)

val mac : unit -> mactype
  (** Returns the mac type employed. *)

(**/**)
val setbps : float -> unit
val bps : unit -> float
  (** Returns the transmission rate (in bits per second) of the MAC layer
    employed. 
    
    HACK. Will change, because this should be a per-node quantity (or even
    per-MAC, if we allow a node to have multiple network interfaces).
  *)
