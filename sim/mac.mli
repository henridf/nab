(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  MAC layer class type and helper functions.
  @author Henri Dubois-Ferriere.
*)


class type mac_t  = 
object
  method objdescr : string
  method recv : ?snr:float -> l2pkt:L2pkt.l2packet_t -> unit -> unit
  method xmit : l2pkt:L2pkt.l2packet_t -> unit
end
    
type mactype = Nullmac | Contmac


val strset_mac : string -> unit
  (** Set the default mac via a string (for example provided as cmdline argument). *)

val mac : unit -> mactype
  (** Returns the mac type employed. *)
