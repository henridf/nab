open Printf
open Coord

type nodeid_t = int
let nid_bcast = max_int
type port_t = int



type time_t = float

module OrderedInt = 
struct
  type t = nodeid_t
  let compare = Pervasives.compare
end

module NodeSet = Set.Make (OrderedInt)
let nodeset_of_list ~nodelist = 
  let rec nodeset_of_list_ set l = match l with 
    | a :: b -> nodeset_of_list_ (NodeSet.add a set) b
    | [] -> set
  in 
  nodeset_of_list_ NodeSet.empty nodelist


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
    | "randomwalk" | "rndwlk" | "rw" -> RANDOMWALK
    | "waypoint" | "wp" -> WAYPOINT
    | other -> raise (Failure  (Printf.sprintf "Unknown Mobility pattern %s" other))
)

let string_of_mobility s = (
  match s with
    | RANDOMWALK -> "random walk"
    | WAYPOINT -> "waypoint"
)

type topology = DISCRETE | CONTINUOUS_TORUS | CONTINUOUS_REFLECTIVE
let topology_of_string s = (
  match s with
    | "d" | "disc" | "discrete" -> DISCRETE
    | "cr" | "contref" | "continuousreflective" -> CONTINUOUS_REFLECTIVE
    | "ct" | "conttor" | "continuoustorus" -> CONTINUOUS_TORUS
    | other -> raise (Failure (Printf.sprintf "Unknown Topology %s" other))
)

let string_of_topology t = (
  match t with
    | DISCRETE -> "Discrete"
    | CONTINUOUS_TORUS -> "Continuous Torus"
    | CONTINUOUS_REFLECTIVE -> "Reflective Torus"
)

type action = COMPUTE_ROUTES | SHOW_ROUTES | SHOW_GRAD
let action_of_string s = (
  match s with
    | "cr" | "croutes" | "computer" | "computeroutes" -> COMPUTE_ROUTES
    | "sr" | "sroutes" | "showr" | "showroutes" -> SHOW_ROUTES
    | "sg" | "sgrad" | "showg" | "showgrad" -> SHOW_GRAD
    | other -> raise (Failure (Printf.sprintf "Unknown Action %s" other))
)

let string_of_action t = (
  match t with
    | COMPUTE_ROUTES -> "Compute Routes"
    | SHOW_ROUTES -> "Show Routes"
    | SHOW_GRAD -> "Show Gradient"
)

type enc_t = {
  mutable t: time_t; 
  mutable p: coordf_t
}
    
let time = ref 0.0

let set_time t = (
  if ((floor (t /. 10.0)) <> (floor (!time /. 10.0))) then (
    Printf.printf "Time: %f\n" t;
    flush stdout;
  );
  time := t
)
let get_time () = !time
  
let enc ~time ~place = {t = time; p = place}
let enc_age  m = (get_time ()) -. m.t






