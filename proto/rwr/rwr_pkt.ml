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

type cost_t = float

let cost_size = 2


type radv = {
  hop_dist : int;           (* # hops *)
  lpl_rwr_cost : cost_t;    (* cost with lpl, RWR *)
  lpl_cost : cost_t         (* cost with lpl, shortest path *)
}

(* must change clone if any mutables get introduced!! *)
type t =  
  | DATA
  | ADV of radv
 
let hdr_size  = function
  | DATA -> 0
  | ADV _ -> Pkt_common.addr_size + cost_size


let clone rwr_pkt = rwr_pkt 


let make_adv_hdr radv =  ADV radv
