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

  method get_nodes_within_radius :  node:Node.node_t -> radius:float -> Common.nodeid_t list

  method dist_coords :  Coord.coordf_t -> Coord.coordf_t -> float
    (* Compute distance between two euclidean coordinates. *)

  method dist_nodes :  Node.node_t -> Node.node_t -> float
    (* Compute distance between two nodes. *)

  method dist_nodeids :  Common.nodeid_t -> Common.nodeid_t -> float
    (* Compute distance between two nodes given by index. *)
    
  method neighbors : Node.node_t -> Node.node_t -> bool

  method update_pos : node:Node.node_t -> oldpos_opt:(Coord.coordf_t option) ->  unit
    (*  After a node has moved, it should inform the world through this method.
	the world will then update the node and its neighbors views of their
	neighbors as necessary.
	~oldpos should only be None if the node is new.
    *)

  method neighbors_consistent : bool

  method find_closest : src:Node.node_t -> f:(Node.node_t -> bool) -> nodeid_t
    (* returns the closest node to src which satisfies the boolean f *)

  method project_2d : Coord.coordf_t -> Coord.coordf_t 
    (* return position projected to a unit square *)

  method scale_unit : float -> float 
    (* return coordinate scaled to [0;1] interval. 
       can be used for example for drawing search disks *)

  method boundarize : Coord.coordf_t -> Coord.coordf_t
    (* A topology-specific function which should put a point back within the
       boundaries of the world, if it has stepped outside, and should return
       the point intact if it has not.
       For example this might be wrapping in a torus, or bouncing on
       reflective borders, etc *)

  method get_nodes_at : Coord.coordf_t -> int list 
    (* returns nodes at given position which is projected to unit square *)

  method sprint_info : unit -> string
end
  

