(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* World: All computation requiring omniscience is done here. 
   Things like:
     - finding neighbors of a node
     - computing delaunay triangulation
     - shortest path
     - etc..

*)

open Misc
open Common

class type world_t = 
object
    
  method random_pos : Coord.coordf_t 
    (* return a uniform random position *)

  method get_nodes_within_radius :  nid:Common.nodeid_t -> radius:float -> Common.nodeid_t list

  method dist_coords :  Coord.coordf_t -> Coord.coordf_t -> float
    (* Compute distance between two euclidean coordinates. *)

  method dist_nodeids :  Common.nodeid_t -> Common.nodeid_t -> float
    (* Compute distance between two nodes given by index. *)
    
  method neighbors : Common.nodeid_t -> Common.nodeid_t list

  method are_neighbors : Common.nodeid_t -> Common.nodeid_t -> bool

  method add_new_ngbr_hook : Common.nodeid_t -> hook:(Common.nodeid_t -> unit) -> unit
    (* Any agent (routing, application, etc) who wants to know when nodes
       enter the neighborhood of the node given as first argument should
       register through this hook. 
    *)

  method init_pos : nid:Common.nodeid_t -> pos:(Coord.coordf_t) -> unit
    (* Set initial position of node nid *)

  method movenode : 
    nid:Common.nodeid_t -> 
    newpos:Coord.coordf_t ->
    unit
    (*  After a node has moved, it should inform the world through this method.
	the world will then update the node and its neighbors views of their
	neighbors as necessary.
	~oldpos should only be None if the node is new.
    *)

  method nodepos : Common.nodeid_t -> Coord.coordf_t 

  method neighbors_consistent : bool

  method find_closest : pos:Coord.coordf_t -> f:(Common.nodeid_t -> bool) ->
    Common.nodeid_t option
    (* returns the closest node to pos which satisfies the boolean f, or None 
       if f never satisfied.
    *)

  method project_2d : Coord.coordf_t -> Coord.coordf_t 
    (* return position projected to a unit square *)

(*  method scale_unit : float -> float 
    return coordinate scaled to [0;1] interval. 
       can be used for example for drawing search disks *)

  method boundarize : Coord.coordf_t -> Coord.coordf_t
    (* A topology-specific function which should put a point back within the
       boundaries of the world, if it has stepped outside, and should return
       the point intact if it has not.
       For example this might be implemented as wrapping in a torus, or
       bouncing on reflective borders, etc 
       This allows a mobility process computing a next position to be unaware
       of the topology limits. *)
    

  method get_node_at : unitpos:Coord.coordf_t -> Common.nodeid_t 
    (* returns closest node to unit-scaled position *)

end
  

