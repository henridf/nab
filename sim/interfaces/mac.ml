



open Misc 

class type t  = 
object

  inherit Log.inheritable_loggable 
  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method xmit : l2pkt:L2pkt.t -> unit
  method bps : float
end
    
type mactype = Nullmac | Contmac

let mac_ = ref Nullmac

let str2mac s = 
  match s with 
    | "null" |  "nullmac" -> Nullmac
    | "contention" | "cont" | "contmac" -> Contmac
    | _ -> raise (Failure "Invalid format for mac type")


let mac_of_string s = match s with 
  | "null" | "nullmac" -> Mac.Nullmac
  | "contention" | "contmac" | "contentionmac" -> Mac.Contmac
  | _ -> raise (Failure ("Invalid mactype "^s))

let strset_mac s = 
  mac_ := str2mac s

let mac() = !mac_


