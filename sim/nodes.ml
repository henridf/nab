(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc


let nodes_array = ref [||]
let set_nodes arr = nodes_array := arr


let iter f = Array.iter f !nodes_array
let map f = Array.map f !nodes_array
let fold f init = Array.fold_right f !nodes_array init

let node i = !nodes_array.(i)



  
  

  


  
