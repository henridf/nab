




open Common


open Misc

let (lazy_world_ : Worldt.lazy_world_t option ref) = ref None 
let w () = try o2v !lazy_world_ with
  | Failure _ -> raise (Failure "World.w() : no instance has been set")
let set_lazy_world t = lazy_world_ := Some t

let (greedy_world_ : Worldt.greedy_world_t option ref) = ref None 
let gw () = try o2v !greedy_world_ with
  | Failure _ -> raise (Failure "World.gw() : no instance has been set")

let set_greedy_world t = 
  greedy_world_ := Some t; 
  lazy_world_ := Some (t :> Worldt.lazy_world_t)
