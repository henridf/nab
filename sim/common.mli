open Coord

type route_algorithm = EASE | GREASE | GRADIENT
val algo_of_string : string -> route_algorithm
val string_of_algo : route_algorithm -> string


type mobility_pattern = RANDOMWALK | WAYPOINT
val mobility_of_string : string -> mobility_pattern
val string_of_mobility : mobility_pattern -> string

type topology = DISCRETE | CONTINUOUS
val topology_of_string : string -> topology 
val string_of_topology : topology -> string

type action = COMPUTE_ROUTES | SHOW_ROUTES | SHOW_GRAD
val action_of_string : string -> action
val string_of_action : action -> string

type meeting = {
  mutable t: int; 
  mutable p: coordf_t
}
    
type ler_data_t = {
  mutable pos :  coordf_t array;
  mutable db :  meeting array array;
}

val meeting : int -> Coord.coordf_t -> meeting
val meeting_age : meeting -> int

val set_time : int -> unit
val get_time : unit -> int
val tick_time : unit -> unit
