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
type 'a coord_t = ('a * 'a)
type coordi_t = int coord_t
type coordf_t = float coord_t
type coordn_t = int array

let (+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let (+++.) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)

let (---) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let (---.) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)


let ( *** ) (x, y) scalar = (x * scalar, y * scalar)
let ( ***. ) (x, y) scalar = (x *. scalar, y *. scalar)

let ( /// ) (x, y) scalar = (x / scalar, y / scalar)
let ( ///. ) (x, y) scalar = (x /. scalar, y /. scalar)


let i2f (x, y) = (float x, float y)
let i2n (x, y) = [|  x; y|]
let f2i (x, y) = (Misc.f2i x, Misc.f2i y)
let f2n (x, y) = [|Misc.f2i x; Misc.f2i y|]
let round (x, y) = (round x, round y)
let floor (x, y) = (floor x, floor y)

let xx (x, _) = x
let yy (_, y) = y


let normi_sq (x, y) = 
  (powi ~num:x ~exp:2) + (powi ~num:y ~exp:2)
    
let normi c = 
  sqrt (Misc.i2f (normi_sq c))

let disti_sq  (x1, y1) (x2, y2) = 
  (powi ~num:(x1 - x2) ~exp:2) + (powi ~num:(y1 - y2) ~exp:2)

let disti c1 c2 = (
  sqrt (Misc.i2f (disti_sq c1 c2))
)

let norm_sq (x, y) =  (x ** 2.0) +. (y ** 2.0)
let norm c = sqrt (norm_sq c)

let dist_sq  (x1, y1) (x2, y2) = 
  ((x1 -. x2) ** 2.0) +. ((y1 -. y2) ** 2.0)

let dist c1 c2 = (
  sqrt  (dist_sq c1 c2)
)

let normalize p = 
  let n = norm p in
  match n with 
    | 0.0 -> p
    | norm -> p ///. norm

let print (x, y) = 
  Printf.printf "<%d, %d>" x y

let sprint (x, y) =   Printf.sprintf "<%d, %d>" x y

let sprintf (x, y) = Printf.sprintf  "<%.3f, %.3f>" x y
