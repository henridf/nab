(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)







type t = {
  mutable enc_age : Time.time_t;
  mutable anchor_pos : Coord.coordf_t;
  mutable search_dist : float;
}

let make_ease_hdr
  ~enc_age
  ~anchor_pos
  =  {
    enc_age=enc_age;
    anchor_pos=anchor_pos;
    search_dist=0.0
  }
	
let clone ease_pkt = {ease_pkt with enc_age=ease_pkt.enc_age}
let anchor ease_hdr = ease_hdr.anchor_pos
let enc_age ease_hdr = ease_hdr.enc_age
let search_dist ease_hdr = ease_hdr.search_dist

let set_search_dist ease_hdr d = 
  ease_hdr.search_dist <- d

let set_anchor_pos ease_hdr anch = 
  ease_hdr.anchor_pos <- anch

let set_enc_age ease_hdr age = 
  ease_hdr.enc_age <- age
