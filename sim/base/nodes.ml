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


let nodes_array = ref [||] (* array of Node.node objects. *)

let check() = if !nodes_array = [||] then
  Log.log#log_warning (lazy "Node iterator called but node array is empty")

let set_nodes arr = nodes_array := arr

let iter f = check(); Array.iter f !nodes_array
let iteri f = check(); Array.iteri f !nodes_array


    
let map f = check(); Array.map f !nodes_array
let mapi f = check(); Array.mapi (fun i _ -> f i) !nodes_array
let fold f init = check(); Array.fold_right f !nodes_array init

let node i = !nodes_array.(i)


