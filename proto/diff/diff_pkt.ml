open Pkt_common

(* L3 STUFF *)
type diff_flags_t = 
    DIFF_DATA | DIFF_RADV

type t = {
  mutable diff_flags : diff_flags_t;
  ssn : int;         (* Source Seqno: All *)
  mutable shc : int; (* Source hopcount: All *)
}

let hdr_size pkt = (* too lazy to differentiat btw types right now, so
			     just putting the 'average' size *)
  raise Misc.Not_Implemented

let clone diff_pkt = {diff_pkt with ssn=diff_pkt.ssn}


let flags diff_pkt = 
  diff_pkt.diff_flags

let ssn diff_pkt = diff_pkt.ssn
let shc diff_pkt = diff_pkt.shc

let incr_shc_pkt diff_pkt  = 
  diff_pkt.shc <- diff_pkt.shc + 1

let decr_shc_pkt diff_pkt  = 
  diff_pkt.shc <- diff_pkt.shc - 1

let make_diff_hdr ~flags ~ssn ~shc = 
  {
    diff_flags=flags;
    ssn=ssn;
    shc=shc;
  }

