(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc 

class type mac_t  = 
object
  method objdescr : string
  method recv : ?snr:float -> l2pkt:L2pkt.t -> unit -> unit
  method xmit : l2pkt:L2pkt.t -> unit
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


let bps_ = ref None
let setbps bps = 
  match !bps_ with
    | None -> bps_ := Some bps
    | Some x -> 
	if x <> bps then 
	  failwith "setbps, odd that two MACs have different speed"



let bps() = o2v !bps_
