



open L3pkt 

let _ADDR_SIZE = 4
let _TTL_SIZE = 1
let _SEQNO_SIZE = 4
let _FLOAT_SIZE = 8

(* L2 STUFF *)

type l2_dst_t = L2_BCAST | L2_DST of Common.nodeid_t

type l2hdr_t = {
  l2src: Common.nodeid_t;
  l2dst: l2_dst_t
}

(* 20 should vaguely represent the mac-layer framing bytes *) 
let l2hdr_size = 20 + 2 * _ADDR_SIZE 

let clone_l2hdr ~l2hdr = {l2hdr with l2src = l2hdr.l2src}

type t = {
  l2hdr : l2hdr_t;
  l3pkt : L3pkt.t
}

let l2pkt_size ~l2pkt = 
  l2hdr_size +
  l3pkt_size ~l3pkt:l2pkt.l3pkt

let clone_l2pkt ~l2pkt = {
  l2hdr=clone_l2hdr ~l2hdr:l2pkt.l2hdr;
  l3pkt=clone_l3pkt ~l3pkt:l2pkt.l3pkt;
}

let l3pkt ~(l2pkt:t) = l2pkt.l3pkt

let l2hdr ~(pkt:t) = pkt.l2hdr
let l2src ~(pkt:t) = (l2hdr pkt).l2src
let l2dst ~(pkt:t) = (l2hdr pkt).l2dst
let string_of_l2dst l2dst = 
  match l2dst with 
    | L2_BCAST -> "L2_bcast"
    | L2_DST d -> string_of_int d

let make_l2pkt ~srcid ~l2_dst ~l3pkt = 
  let l2hdr = {l2src=srcid; l2dst=l2_dst} 
  in {l2hdr=l2hdr; l3pkt=l3pkt}



