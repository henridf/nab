(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


open Misc

open Common

class type world_t = 
object
    
  method random_pos : Coord.coordf_t 
    (** Return a uniform random position over the surface of the simulated area *)

  method get_nodes_within_radius :  nid:Common.nodeid_t -> radius:float -> Common.nodeid_t list
    (** Return all nodes within a given distance of a node.*)

  method dist_coords :  Coord.coordf_t -> Coord.coordf_t -> float
    (** Compute distance between two euclidean coordinates. *)

  method dist_nodeids :  Common.nodeid_t -> Common.nodeid_t -> float
    (** Compute distance between two nodes. *)
    
  method neighbors : Common.nodeid_t -> Common.nodeid_t list
    (** Return all the neighbors of a node. Neighbors are nodes within
      distance {!Params.rrange} of each other. *)

  method are_neighbors : Common.nodeid_t -> Common.nodeid_t -> bool
    (** Return true if two nodes are neighbors, false otherwise.
      Neighbors are nodes within  distance {!Params.rrange} of each other. *)

  method add_new_ngbr_hook : Common.nodeid_t -> hook:(Common.nodeid_t -> unit) -> unit
    (** Any agent (routing, application, etc) who wants to know when nodes
      enter the neighborhood of the node given as first argument should
      register through this hook. 
    *)

  method add_mob_mhook : hook:(Coord.coordf_t -> Common.nodeid_t -> unit) -> unit
    (** Any monitoring application can register here to receive update each
       time the node moves.
       If multiple apps, order in which called is unspecified.*)
    
  method init_pos : nid:Common.nodeid_t -> pos:(Coord.coordf_t) -> unit
    (** Set initial position of a node. This should only be used once, after a
      node object has been created. *)

  method movenode : 
    nid:Common.nodeid_t -> 
    newpos:Coord.coordf_t ->
    unit
    (**  Each time a node moves, the World.object should be informed through
      this method.
    *)

  method nodepos : Common.nodeid_t -> Coord.coordf_t 
    (** Return the position of a node. *)

  method find_closest : pos:Coord.coordf_t -> f:(Common.nodeid_t -> bool) ->
    Common.nodeid_t option
    (** Returns the closest node to pos which satisfies the boolean f, or None 
      if f never satisfied. 
      A common use is to pass the function [~f:(fun _ -> true)] in order to
      find the node closest to a point.
    *)

    
  method is_connected : unit -> bool
    (** Returns true if the network is connected (under a boolean connectivity
      model, with connectivity up to Params.rrange), false otherwise.
      Warning: this is SLOW for networks in the 100s of nodes or above. *)

  (**/**)

  method neighbors_consistent : bool

  method boundarize : Coord.coordf_t -> Coord.coordf_t
    (* A topology-specific function which should put a point back within the
       boundaries of the World. if it has stepped outside, and should return
       the point intact if it has not.
       For example this might be implemented as wrapping in a torus, or
       bouncing on reflective borders, etc 
       This allows a mobility process computing a next position to be unaware
       of the topology limits. *)
    
  method project_2d : Coord.coordf_t -> Coord.coordf_t 
    (* return position projected to a unit square *)
    
  (*  method scale_unit : float -> float 
      return coordinate scaled to [0;1] interval. 
      can be used for example for drawing search disks *)
end
  

open Misc

let (world_:world_t option ref) = ref None 
let w () = try o2v !world_ with
  | Failure _ -> raise (Failure "World.w() : no instance has been set")

let set_world t = world_ := Some t