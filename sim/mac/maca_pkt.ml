type cts = {cts_dst: Common.nodeid_t; cts_len : int}
    (* cts_dst is the node which is now cleared to send a packet *)
       

type rts = {rts_dst: Common.nodeid_t; rts_len : int}
    (* rts_dst is the node to which we want to send data *)

type t = RTS of rts | CTS of cts | DATA 

let make_rts ~dst ~len = {rts_dst=dst; rts_len=len}
let make_cts ~dst ~len = {cts_dst=dst; cts_len=len}

let clone p = p
let size p = 1 + (* 1 for encoding pkttype, really 2 bits is enough. *)
  match p with
      RTS _ -> 2
    | CTS _ -> 2
    | DATA -> 0

let string_of_maca_pkt = function
  | RTS {rts_dst=d; rts_len=l} -> Printf.sprintf "RTS: dst %d, len %d" d l
  | CTS {cts_dst=d; cts_len=l} -> Printf.sprintf "CTS: dst %d, len %d" d l
  | DATA -> "DATA"
