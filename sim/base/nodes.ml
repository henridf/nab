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







open Misc


let nodes_array = ref ([||]: Simplenode.simplenode array)

let check() = if !nodes_array = [||] then
  Log.log#log_warning (lazy "Node iterator called but node array is empty")

let set_nodes arr = nodes_array := arr

let iter f = check(); Array.iter f !nodes_array
let iteri f = check(); Array.iteri f !nodes_array


    
let map f = check(); Array.map f !nodes_array
let mapi f = check(); Array.mapi (fun i _ -> f i) !nodes_array
let fold f init = check(); Array.fold_right f !nodes_array init

let node i = !nodes_array.(i)


let gpsnodes_array = ref ([||]: Gpsnode.gpsnode array)

let check_gps() = 
  if !gpsnodes_array = [||] then 
  Log.log#log_warning (lazy "GPSNode iterator called but gpsnode array is empty")

let set_gpsnodes arr = 
  gpsnodes_array := arr;
  nodes_array := Array.map (fun gpsnode -> (gpsnode :> Simplenode.simplenode)) !gpsnodes_array

let gpsiter f = check_gps(); Array.iter f !gpsnodes_array
let gpsiteri f = check_gps(); Array.iteri f !gpsnodes_array
let gpsmap f = check_gps(); Array.map f !gpsnodes_array
let gpsmapi f = check_gps(); Array.mapi (fun i _ -> f i) !gpsnodes_array
let gpsfold f init = check_gps(); Array.fold_right f !gpsnodes_array init

let gpsnode i = !gpsnodes_array.(i)



  
  

  


  


  
  

  


  

