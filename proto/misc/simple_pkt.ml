open Pkt_common

type t = {
  seqno : int;         
}


let hdr_size pkt =  _SEQNO_SIZE

let clone simple_pkt = {simple_pkt with seqno=simple_pkt.seqno}

let seqno simple_pkt = simple_pkt.seqno

let make_simple_hdr  ~seqno   =  {seqno=seqno}
