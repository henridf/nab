

open Pkt_common

type t = {
  distance : int;         
}


let hdr_size _ =  int_size

let clone dbf_pkt = {dbf_pkt with distance=dbf_pkt.distance}

let distance dbf_pkt = dbf_pkt.distance

let make_dbf_hdr  ~distance   =  {distance=distance}
