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







let addr_size = 4
let ttl_size = 1
let seqno_size = 4
let float_size = 8


(* L4 (APPLICATION) STUFF *)

type hello_payload_t =  Coord.coordf_t
let hello_payload_size = 2 * float_size


type t = 
    (* if any l4 payload becomes mutable, need to 
       change clone_l4pkt below *)
    [ `EMPTY
    | `APP_PKT of int
    | `HELLO_PKT of hello_payload_t
    ]
      
let clone_l4pkt ~l4pkt = l4pkt

let l4pkt_size ~l4pkt = 
  match l4pkt with
    | `APP_PKT _ -> 1500
    | `EMPTY -> 0
    | `HELLO_PKT _ -> hello_payload_size
	
