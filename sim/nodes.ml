(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

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

let set_gpsnodes arr = 
  gpsnodes_array := arr;
  nodes_array := Array.map (fun gpsnode -> (gpsnode :> Simplenode.simplenode)) !gpsnodes_array

let gpsiter f = Array.iter f !gpsnodes_array
let gpsmap f = Array.map f !gpsnodes_array
let gpsmapi f = Array.mapi (fun i _ -> f i) !gpsnodes_array
let gpsfold f init = Array.fold_right f !gpsnodes_array init

let gpsnode i = !gpsnodes_array.(i)



  
  

  


  


  
  

  


  

