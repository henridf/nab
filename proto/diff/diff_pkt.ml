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

