(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

class type mac_t  = 
object
  method objdescr : string
  method recv : ?snr:float -> l2pkt:L2pkt.l2packet_t -> unit -> unit
  method xmit : l2pkt:L2pkt.l2packet_t -> unit
end
    
type mactype = Nullmac | Contmac

let mac_ = ref Nullmac

let str2mac s = 
  match s with 
    | "null" |  "nullmac" -> Nullmac
    | "contention" | "cont" | "contmac" -> Contmac
    | _ -> raise (Failure "Invalid format for mac type")


let strset_mac s = 
  mac_ := str2mac s

let mac() = !mac_
