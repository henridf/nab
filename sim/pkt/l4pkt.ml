



let _ADDR_SIZE = 4
let _TTL_SIZE = 1
let _SEQNO_SIZE = 4
let _FLOAT_SIZE = 8


(* L4 (APPLICATION) STUFF *)

type hello_payload_t =  Coord.coordf_t
let hello_payload_size = 2 * _FLOAT_SIZE


type t = 
    (* if any l4 payload becomes mutable, need to 
       change clone_l4pkt below *)
    [ `NONE
    | `APP_PKT
    | `HELLO_PKT of hello_payload_t
    ]
      
let clone_l4pkt ~l4pkt = l4pkt

let l4pkt_size ~l4pkt = 
  match l4pkt with
    | `APP_PKT -> 1500
    | `NONE -> 0
    | `HELLO_PKT _ -> hello_payload_size
	
