open Misc

let _LINK_RANGE = 250.0
let one_meter = (1.0 /. _LINK_RANGE ) 

let (world_:World.world_t option ref) = ref None 
let world () = try o2v !world_ with
  | Failure a -> raise (Failure "Gworld.world() : no instance has been set")

let set_world t = world_ := Some t
