(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  Mac_t: Class type for MAC layers.
  @author Henri Dubois-Ferriere.
*)

class type mac_t  = 
object
  method objdescr : string
  method recv : ?snr:float -> l2pkt:L2pkt.l2packet_t -> unit -> unit
  method xmit : l2pkt:L2pkt.l2packet_t -> unit
end
    
