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







type t = {
  mutable enc_age : Time.time_t;
  mutable anchor_pos : Coord.coordf_t;
  mutable search_dist : float;
}

let make_ler_hdr
  ~enc_age
  ~anchor_pos
  =  {
    enc_age=enc_age;
    anchor_pos=anchor_pos;
    search_dist=0.0
  }
	
let clone ler_pkt = {ler_pkt with enc_age=ler_pkt.enc_age}
let anchor ler_hdr = ler_hdr.anchor_pos
let enc_age ler_hdr = ler_hdr.enc_age
let search_dist ler_hdr = ler_hdr.search_dist

let set_search_dist ler_hdr d = 
  ler_hdr.search_dist <- d

let set_anchor_pos ler_hdr anch = 
  ler_hdr.anchor_pos <- anch

let set_enc_age ler_hdr age = 
  ler_hdr.enc_age <- age
