(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc


let nodes_array = ref ([||]: Simplenode.simplenode array)

let set_nodes arr = nodes_array := arr

let iter f = Array.iter f !nodes_array
let iteri f = for i = 0 to Array.length !nodes_array - 1 do  f i done


    
let map f = Array.map f !nodes_array
let fold f init = Array.fold_right f !nodes_array init

let node i = !nodes_array.(i)


let gpsnodes_array = ref ([||]: Gpsnode.gpsnode array)

let set_gpsnodes arr = 
  gpsnodes_array := arr;
  nodes_array := Array.map (fun gpsnode -> (gpsnode :> Simplenode.simplenode)) !gpsnodes_array

let gpsiter f = Array.iter f !gpsnodes_array
let gpsmap f = Array.map f !gpsnodes_array
let gpsfold f init = Array.fold_right f !gpsnodes_array init

let gpsnode i = !gpsnodes_array.(i)



  
  

  


  


  
  

  


  

