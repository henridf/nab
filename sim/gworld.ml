open Misc
let (world_:World.world_t option ref) = ref None 
let world () = o2v !world_ 
let set_world t = world_ := Some t
