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







let _ADDR_SIZE = 4
let _TTL_SIZE = 1
let _SEQNO_SIZE = 4
let _FLOAT_SIZE = 8


(* L4 (APPLICATION) STUFF *)

type hello_payload_t =  Coord.coordf_t
let hello_payload_size = 2 * _FLOAT_SIZE


type t = 
    (* if any l4 payload becomes mutable, need to 
       change clone_l4pkt below *)
    [ `NONE
    | `APP_PKT
    | `HELLO_PKT of hello_payload_t
    ]
      
let clone_l4pkt ~l4pkt = l4pkt

let l4pkt_size ~l4pkt = 
  match l4pkt with
    | `APP_PKT -> 1500
    | `NONE -> 0
    | `HELLO_PKT _ -> hello_payload_size
	
