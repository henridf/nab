(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open L4pkt
open Pkt_common

(* IMPORTANT: WHEN CHANGING *ANY* HEADER/PACKET FIELDS, 
   MAKE SURE THAT ALL THE APPROPRIATE *_size FUNCTIONS REMAIN VALID *) 

open Misc

let _L3_BCAST_ADDR = (* 255.255.255.255 *)
  255 + 
  powi ~num:2 ~exp:8 + 
  powi ~num:2 ~exp:16 + 
  powi ~num:2 ~exp:24



type l3hdr_ext_t = 
    [ `NONE
    | `EASE_HDR of Ease_pkt.t
    | `GREP_HDR of Grep_pkt.t
    | `AODV_HDR of Aodv_pkt.t
    | `DIFF_HDR of Diff_pkt.t
    ]

let clone_l3hdr_ext = function
  | `NONE -> `NONE
  | `EASE_HDR e -> `EASE_HDR (Ease_pkt.clone e)
  | `GREP_HDR e -> `GREP_HDR (Grep_pkt.clone e)
  | `AODV_HDR e -> `AODV_HDR (Aodv_pkt.clone e)
  | `DIFF_HDR e -> `DIFF_HDR (Diff_pkt.clone e)


(* note on TTL:
   a ttl is decremented before sending a packet. 
   if decrementing makes it -1 then packet is dropped.
   for example, a 1-hop broadcast has ttl 1, which is decremented to 0 before
   being sent, so that receiving nodes will not forward it further *)

(** A l3hdr contains a src, dst, ttl and maybe some protocol-specific
   extensions *)
type l3hdr_t = {(* adjust l3hdr_size if this changes *)
  src : Common.nodeid_t;
  dst : Common.nodeid_t;
  mutable ttl : int;
  ext : l3hdr_ext_t
}

let clone_l3hdr ~l3hdr = {l3hdr with src = l3hdr.src; ext=(clone_l3hdr_ext l3hdr.ext) }

let l3hdr_ext_size = function
  | `NONE -> 0
  | `EASE_HDR _ -> 3 * _FLOAT_SIZE (* enc. age, pos *)
  | `GREP_HDR hdr ->  Grep_pkt.hdr_size hdr 
  | `AODV_HDR hdr ->  Aodv_pkt.hdr_size hdr 
  | `DIFF_HDR hdr ->  Diff_pkt.hdr_size hdr 


 
let l3hdr_size ~l3hdr = 
  2 * _ADDR_SIZE (* src, dst *)
  + _TTL_SIZE    (* ttl *)
  +  (l3hdr_ext_size l3hdr.ext)
    

	
type t = {
  l3hdr : l3hdr_t;
  l4pkt : L4pkt.t
}

let l4pkt ~(l3pkt:t) = l3pkt.l4pkt

let l3pkt_size ~l3pkt = 
  l3hdr_size ~l3hdr:l3pkt.l3hdr +
  l4pkt_size ~l4pkt:l3pkt.l4pkt

let clone_l3pkt ~l3pkt = {
  l3hdr=clone_l3hdr ~l3hdr:l3pkt.l3hdr;
  l4pkt=clone_l4pkt ~l4pkt:l3pkt.l4pkt;
}

let l3hdr ~(l3pkt:t) = l3pkt.l3hdr
let l3src ~(l3pkt:t) = (l3hdr l3pkt).src
let l3dst ~(l3pkt:t) = (l3hdr l3pkt).dst
let l3ttl ~(l3pkt:t) = (l3hdr l3pkt).ttl
let set_l3ttl ~(l3pkt:t) ~ttl = (l3hdr l3pkt).ttl <- ttl

let decr_l3ttl ~(l3pkt:t) = 
  l3pkt.l3hdr.ttl <- l3pkt.l3hdr.ttl - 1


let ease_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `EASE_HDR e -> e
    | _ -> raise (Failure "Packet.ease_hdr")

let grep_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `GREP_HDR e -> e
    | _ -> raise (Failure "Packet.grep_hdr")

let aodv_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `AODV_HDR e -> e
    | _ -> raise (Failure "Packet.aodv_hdr")

let diff_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `DIFF_HDR e -> e
    | _ -> raise (Failure "Packet.diff_hdr")

let get_ease_l3ext (l3hdr:l3hdr_t) = 
  match l3hdr.ext with
    | `EASE_HDR e -> e
    | _ -> raise (Failure "Packet.get_ease_l3ext")


let make_l3hdr 
  ~srcid 
  ~dstid 
  ?(ttl=255)
  ?(ext=`NONE)
  ()
  = {
    src=srcid; 
    dst=dstid;
    ttl=ttl;
    ext=ext
  }
  

let make_app_pkt ~l3hdr = {
  l3hdr=l3hdr;
  l4pkt=`APP_PKT
}

let make_l3pkt ~l3hdr ~l4pkt = {
  l3hdr=l3hdr;
  l4pkt=l4pkt
}

  
