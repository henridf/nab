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


open Misc
open L3pkt 
open Pkt_common

(* L2 STUFF *)

let l2_bcast_addr = (* 255.255.255.255 *)
  255 + 
  powi ~num:2 ~exp:8 + 
  powi ~num:2 ~exp:16 + 
  powi ~num:2 ~exp:24

type l2hdr_ext_t = 
    NONE | MACAW of Macaw_pkt.t | MACA of Maca_pkt.t


type l2hdr_t = {
  l2src: Common.nodeid_t;
  l2dst: Common.nodeid_t;
  ext: l2hdr_ext_t;
}

let clone_l2hdr_ext = function
  | NONE -> NONE
  | MACAW e -> MACAW (Macaw_pkt.clone e)
  | MACA e -> MACA (Maca_pkt.clone e)

let l2hdr_ext_size = function
  | NONE -> 0
  | MACAW e -> Macaw_pkt.size e
  | MACA e -> Maca_pkt.size e

(* 8 should vaguely represent the phy-layer framing bytes *) 
let l2hdr_size hdr = 8 + 2 * addr_size + (l2hdr_ext_size hdr.ext)

let clone_l2hdr ~l2hdr = {l2hdr with ext=clone_l2hdr_ext l2hdr.ext}

type t = {
  l2hdr : l2hdr_t;
  l3pkt : L3pkt.t
}

let l2pkt_size ~l2pkt = 
  l2hdr_size l2pkt.l2hdr +
  l3pkt_size l2pkt.l3pkt

let clone_l2pkt ~l2pkt = l2pkt
(*{
  l2hdr=clone_l2hdr ~l2hdr:l2pkt.l2hdr;
  l3pkt=clone_l3pkt l2pkt.l3pkt;
}*)

let l3pkt l2pkt = l2pkt.l3pkt

let l2hdr pkt = pkt.l2hdr
let l2src pkt = pkt.l2hdr.l2src
let l2dst pkt = pkt.l2hdr.l2dst
let string_of_l2dst l2dst = 
  if l2dst = l2_bcast_addr then "L2_bcast"
  else string_of_int l2dst

let l2hdr_ext l2pkt = l2pkt.l2hdr.ext

let maca_hdr l2pkt = 
  match l2pkt.l2hdr.ext with
    | MACA h -> h
    | MACAW _ | NONE -> raise (Failure "L2pkt.maca_hdr")

let make_l2pkt ?(ext=NONE) ~src ~dst l3pkt = 
  let l2hdr = {l2src=src; l2dst=dst; ext=ext} 
  in {l2hdr=l2hdr; l3pkt=l3pkt}



