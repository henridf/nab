(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

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

(* 100 should vaguely represent the mac-layer framing bytes *) 
let l2hdr_size = 100 + 2 * _ADDR_SIZE 

let clone_l2hdr ~l2hdr = {l2hdr with l2src = l2hdr.l2src}

type l2packet_t = {
  l2hdr : l2hdr_t;
  l3pkt : l3packet_t
}

let l2pkt_size ~l2pkt = 
  l2hdr_size +
  l3pkt_size ~l3pkt:l2pkt.l3pkt

let clone_l2pkt ~l2pkt = {
  l2hdr=clone_l2hdr ~l2hdr:l2pkt.l2hdr;
  l3pkt=clone_l3pkt ~l3pkt:l2pkt.l3pkt;
}

let l3pkt ~(l2pkt:l2packet_t) = l2pkt.l3pkt

let l2hdr ~(pkt:l2packet_t) = pkt.l2hdr
let l2src ~(pkt:l2packet_t) = (l2hdr pkt).l2src
let l2dst ~(pkt:l2packet_t) = (l2hdr pkt).l2dst

let make_l2pkt ~srcid ~l2_dst ~l3pkt = 
  let l2hdr = {l2src=srcid; l2dst=l2_dst} 
  in {l2hdr=l2hdr; l3pkt=l3pkt}

