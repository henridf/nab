(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)

open L4pkt
open Pkt_common

(* IMPORTANT: WHEN CHANGING *ANY* HEADER/PACKET FIELDS, 
   MAKE SURE THAT ALL THE APPROPRIATE *_size FUNCTIONS REMAIN VALID *) 

open Misc

let l3_bcast_addr = (* 255.255.255.255 *)
  255 + 
  powi ~num:2 ~exp:8 + 
  powi ~num:2 ~exp:16 + 
  powi ~num:2 ~exp:24

let default_ttl = 255

type l3hdr_ext_t = 
    [ `NONE
    | `LER_HDR of Ler_pkt.t
    | `GREP_HDR of Grep_pkt.t
    | `STR_HDR of Str_pkt.t
    | `AODV_HDR of Aodv_pkt.t
    | `DIFF_HDR of Diff_pkt.t
    | `SIMPLE_HDR of Simple_pkt.t
    ]

let clone_l3hdr_ext = function
  | `NONE -> `NONE
  | `LER_HDR e -> `LER_HDR (Ler_pkt.clone e)
  | `GREP_HDR e -> `GREP_HDR (Grep_pkt.clone e)
  | `STR_HDR e -> `STR_HDR (Str_pkt.clone e)
  | `AODV_HDR e -> `AODV_HDR (Aodv_pkt.clone e)
  | `DIFF_HDR e -> `DIFF_HDR (Diff_pkt.clone e)
  | `SIMPLE_HDR e -> `SIMPLE_HDR (Simple_pkt.clone e)


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
  | `LER_HDR hdr -> Ler_pkt.size hdr
  | `GREP_HDR hdr ->  Grep_pkt.hdr_size hdr 
  | `STR_HDR hdr ->  Str_pkt.hdr_size hdr 
  | `AODV_HDR hdr ->  Aodv_pkt.hdr_size hdr 
  | `DIFF_HDR hdr ->  Diff_pkt.hdr_size hdr 
  | `SIMPLE_HDR hdr ->  Simple_pkt.hdr_size hdr 


 
let l3hdr_size ~l3hdr = 
  2 * addr_size (* src, dst *)
  + ttl_size    (* ttl *)
  +  (l3hdr_ext_size l3hdr.ext)
    

	
type t = {
  l3hdr : l3hdr_t;
  l4pkt : L4pkt.t
}

let l4pkt l3pkt = l3pkt.l4pkt

let l3pkt_size l3pkt = 
  l3hdr_size ~l3hdr:l3pkt.l3hdr +
  l4pkt_size ~l4pkt:l3pkt.l4pkt

let clone_l3pkt l3pkt = {
  l3hdr=clone_l3hdr ~l3hdr:l3pkt.l3hdr;
  l4pkt=clone_l4pkt ~l4pkt:l3pkt.l4pkt;
}

let l3hdr l3pkt = l3pkt.l3hdr
let l3src l3pkt = (l3hdr l3pkt).src
let l3dst l3pkt = (l3hdr l3pkt).dst
let l3ttl l3pkt = (l3hdr l3pkt).ttl

let decr_l3ttl l3pkt = 
  {l3hdr = {l3pkt.l3hdr with ttl = l3pkt.l3hdr.ttl - 1};
  l4pkt = l3pkt.l4pkt}


let l3hdr_ext l3pkt = l3pkt.l3hdr.ext


let ler_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `LER_HDR e -> e
    | _ -> raise (Failure "L3pkt.ler_hdr")

let grep_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `GREP_HDR e -> e
    | _ -> raise (Failure "L3pkt.grep_hdr")

let str_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `STR_HDR e -> e
    | _ -> raise (Failure "L3pkt.str_hdr")

let aodv_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `AODV_HDR e -> e
    | _ -> raise (Failure "L3pkt.aodv_hdr")

let diff_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `DIFF_HDR e -> e
    | _ -> raise (Failure "L3pkt.diff_hdr")

let simple_hdr l3pkt = 
  match l3pkt.l3hdr.ext with
    | `SIMPLE_HDR e -> e
    | _ -> raise (Failure "L3pkt.simple_hdr")

let get_ler_l3ext (l3hdr:l3hdr_t) = 
  match l3hdr.ext with
    | `LER_HDR e -> e
    | _ -> raise (Failure "L3pkt.get_ler_l3ext")


let make_l3hdr 
  ~src 
  ~dst 
  ?(ttl=default_ttl)
  ?(ext=`NONE)
  ()
  = {
    src=src; 
    dst=dst;
    ttl=ttl;
    ext=ext
  }
  



let make_l3pkt ~l3hdr ~l4pkt = {
  l3hdr=l3hdr;
  l4pkt=l4pkt
}

  
