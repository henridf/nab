




open Common
open Misc

type worldtype = 
    Lazy_taurus 
  | Greedy_taurus 
  | Lazy_reflecting 
  | Greedy_reflecting
  | Epfl

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


let str2mac s = 
    match s with 
      | "lazy_taurus" |  "taurus_lazy" 
	  -> Lazy_taurus
      | "greedy_taurus" |  "taurus_greedy" 
	  -> Greedy_taurus
      | "lazy_reflecting" |  "reflecting_lazy" 
	  -> Lazy_reflecting
      | "greedy_reflecting" |  "reflecting_greedy" 
	  -> Greedy_reflecting
      | "epfl" |  "epfl_world" 
	  -> Epfl
      | _ -> raise (Failure ("Invalid worldtype "^s))

