(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* note: 
   look at the deleted file discrTorusTop in CVS repository before continuing
   this one - might contain some useful code *)

(* 
   Initial attempt at 2D lattice, built on a graph.
   Not finished because i realized that i could reuse crworld, simply
   constraining nodes to move over discrete positions. but then, open question
   is: crworld will not respect manhattan distance. is this a problem?? don't
   think so.
   
   2D lattice, built over a graph *)

class dworld ~side : World.world_t = 
object(s)

  val side = side
  val g = Graph.make_lattice_ ~dim:2 ~side:side  
  val mutable node_positions_ =  [|(0, 0)|]

  initializer (
    Log.log#log_always (lazy 
      (sprintf "New Dworld : size %d, #nodes %d" 
      side (Param.get Params.nodes))
    );
  )

  method random_pos = Coord_i2f (Random.int side, Random.int side)


  method nodepos nid = Coord.i2f (node_positions_.(nid))
    (* Returns the lattice position projected to euclidean coords *)

  method private gnodepos nid = Coord.i2n (node_positions_.(nid))
    (* Returns the lattice position as an array (as used by graph) *)

  method get_nodes_within_radius   ~nid ~radius = (
    (Misc.isint radius || 
    raise 
      (Failure "Dworld.get_ndoes_within_radius: radius should be integer"));
    Graph.nhop_and_less_neigbors g (s#gnodepos nid) ~radius:(f2i radius)
  )
      
  method dist_coords a b = (
    ((Coord.coord_floor a = a )
    &&
    (Coord.coord_floor b = b )
    ||
    raise 
      (Failure "Dworld.dist_coords: coords should be integer"));
    Graph.lattice_dist_ g 
      (Coord.coord_f2n  a )
      (Coord.coord_f2n  b) 
  )

  method dist_nodeids id1 id2 =
    Graph.lattice_dist_ g 
      (s#gnodepos id1) (s#gnodepos id2)

(*    
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

*)
end
