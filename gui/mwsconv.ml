(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)






open Misc
open Route

let x_pix_size = ref (Param.get Params.x_pix_size)
let y_pix_size = ref (Param.get Params.y_pix_size)

let init() = (
 x_pix_size :=  (Param.get Params.x_pix_size);
 y_pix_size :=  (Param.get Params.y_pix_size)
)

let x_mtr() = Param.get Params.x_size
and y_mtr() = Param.get Params.y_size

let x_mtr_to_pix x = f2i ((i2f !x_pix_size) *. (x /. x_mtr()))
let y_mtr_to_pix y = f2i ((i2f !y_pix_size) *. (y /. y_mtr()))

let x_pix_to_mtr x = (x_mtr()) *. ((i2f x) /. (i2f !x_pix_size))
let y_pix_to_mtr y = (y_mtr()) *. ((i2f y) /. (i2f !y_pix_size))


let pos_mtr_to_pix pos = 
  (x_mtr_to_pix (Coord.xx pos), y_mtr_to_pix (Coord.yy pos))

let pos_pix_to_mtr pos = 
  (x_pix_to_mtr (Coord.xx pos), y_pix_to_mtr (Coord.yy pos))


let closest_node_at pix_pos = 
  let pos = pos_pix_to_mtr pix_pos in
    o2v ((World.w())#find_closest ~pos ())

let route_mtr_to_pix r = 
  List.map 
    (fun h -> {h with hop=(pos_mtr_to_pix h.hop)})
    r

let route_nodeid_to_pix r = 
  List.map 
    (fun h -> 
      let nodepos = (World.w())#nodepos h.hop in
      {h with hop=(pos_mtr_to_pix nodepos)}
    ) r

let ease_route_mtr_to_pix r = 
  List.map 
    (fun h -> 
      let info = 
	match h.info with 
	  | None -> None
	  | Some i -> Some {i with  anchor = (pos_mtr_to_pix i.anchor)}
      in
      {h with hop=(pos_mtr_to_pix h.hop); 
      info = info}
    )
    r

let ease_route_nodeid_to_pix r = 
  List.map 
    (fun h -> 
      let nodepos = (World.w())#nodepos h.hop in
      let info = 
	match h.info with 
	  | None -> None
	  | Some i -> 
	      let anchorpos = (World.w())#nodepos i.anchor in
	      Some {i with  anchor = (pos_mtr_to_pix anchorpos)}
      in

      {h with hop=(pos_mtr_to_pix nodepos);
	info = info}

    ) r
