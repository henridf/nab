open Printf
open Coord


type route_algorithm = EASE | GREASE | GRADIENT

let algo_of_string s = (
  match s with 
    | "ease" | "EASE" | "Ease" -> EASE
    | "grease" | "GREASE" | "Grease" -> GREASE
    | "gradient" -> GRADIENT
    | other -> raise (Failure (Printf.sprintf "Unknown Algorithm %s" other))
)

let string_of_algo a = (
  match a with 
    | EASE -> "Ease"
    | GREASE -> "Grease"
    | GRADIENT -> "Gradient"
)

type mobility_pattern = RANDOMWALK | WAYPOINT
let mobility_of_string s = (
  match s with
    | "randomwalk" | "rndwlk" -> RANDOMWALK
    | "waypoint" -> WAYPOINT
    | other -> raise (Failure  (Printf.sprintf "Unknown Mobility pattern %s" other))
)
let string_of_mobility s = (
  match s with
    | RANDOMWALK -> "random walk"
    | WAYPOINT -> "waypoint"
)

type topology = DISCRETE | CONTINUOUS
let topology_of_string s = (
  match s with
    | "d" | "disc" | "discrete" -> DISCRETE
    | "c" | "cont" | "continuous" -> CONTINUOUS
    | other -> raise (Failure (Printf.sprintf "Unknown Topology %s" other))
)
let string_of_topology t = (
  match t with
    | DISCRETE -> "Discrete"
    | CONTINUOUS -> "Continuous"
)

type meeting = {
  mutable t: int; 
  mutable p: coordf_t
}
    
type ler_data_t = {
  mutable pos :  coordf_t array;
  mutable db :  meeting array array;
}

let time = ref 0
  
let set_time t = time := t
let get_time () = !time
let tick_time () = incr time
  
let meeting time coord = {t = time; p = coord}
let meeting_age  m = if m.t == -1 then max_int else (get_time ()) - m.t

