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

type time_t = float
type t = time_t
type dtime_t = int
type abs_or_rel_t = ABS of time_t | REL of time_t

let maintain_discrete_time_ = ref false

(* Time *)
let time_ = ref 0.0
let dtime_ = ref 0
let maintain_discrete_time() = 
  maintain_discrete_time_ := true;
  dtime_ := truncate !time_

let set_time t = 
  time_ := t;
  if !maintain_discrete_time_ then 
    dtime_ := truncate !time_

let get_time () = !time_
  
let dtime() = !dtime_



let time = get_time
