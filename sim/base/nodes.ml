



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
let gpsmap f = check_gps(); Array.map f !gpsnodes_array
let gpsmapi f = check_gps(); Array.mapi (fun i _ -> f i) !gpsnodes_array
let gpsfold f init = check_gps(); Array.fold_right f !gpsnodes_array init

let gpsnode i = !gpsnodes_array.(i)



  
  

  


  


  
  

  


  

