open Coord

type nodeid_t = int
val nid_bcast : nodeid_t

type port_t = int

type time_t = float


module NodeSet : Set.S with type elt = nodeid_t
val nodeset_of_list : nodelist:(nodeid_t list) -> NodeSet.t

type route_algorithm = EASE | GREASE | GRADIENT
val algo_of_string : string -> route_algorithm
val string_of_algo : route_algorithm -> string


type mobility_pattern = RANDOMWALK | WAYPOINT
val mobility_of_string : string -> mobility_pattern
val string_of_mobility : mobility_pattern -> string

type topology = DISCRETE | CONTINUOUS_TORUS | CONTINUOUS_REFLECTIVE
val topology_of_string : string -> topology 
val string_of_topology : topology -> string

type action = COMPUTE_ROUTES | SHOW_ROUTES | SHOW_GRAD
val action_of_string : string -> action
val string_of_action : action -> string

type enc_t = {
  mutable t: time_t; 
  mutable p: coordf_t
}
    
val enc : time:time_t -> place:Coord.coordf_t -> enc_t
val enc_age : enc_t -> time_t

val set_time : time_t -> unit
val get_time : unit -> time_t
