(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)







(** Continuous topology with reflective boundaries.
  @author Henri Dubois-Ferriere *)

open Coord
open Pervasives
open Misc
open Common
open Printf
open Graph


(** The base class from which {!Crworld.world_lazy} and
  {!Crworld.world_greedy} inherit.

  @param x X size of World.in meters.
  @param y Y size in meters of World.
  @param rrange Radio range of nodes. Used to determine the coarseness 
  of the discrete grid (quantification) of node positions.
*)
class virtual world_common ~x ~y ~rrange  = (
  object(s)

    val mutable grid_of_nodes_ =  (Array.make_matrix 1 1 ([]:Common.nodeid_t list))
    val mutable node_positions_ =  [|(0., 0.)|]

    method private grid_of_nodes = grid_of_nodes_

    val world_size_x =  x 
    val world_size_y =  y
    val rrange_ = rrange

    (* We assume that nodes inhabit a rectangular surface.
       We tile that surface into rectangular tiles of which are chosen
       as small as possible (under the constraint that they have both sides
       bigger than the radio range. 
       
       So for example, if the world is 15x10, and radio range is 3, we will
       have tiles of size 3x3.3
    *)

    val grid_size_x = f2i (floor (x /. rrange));
    val grid_size_y = f2i (floor (y /. rrange));
    val tile_size_x = x /. (floor (x /. rrange));
    val tile_size_y = y /. (floor (y /. rrange));



    val rrange_sq_ = rrange ** 2.0
      
    val mutable mob_mhooks = []

    val initial_pos = (0.0, 0.0)

      
    (*                                                        *)
    (* The virtual methods which are lazy vs greedy specific. *)
    (*                                                        *)
    method virtual neighbors : Common.nodeid_t -> Common.nodeid_t list
      (* See {!Worldt.lazy_world_t.neighbors}.
	 Virtual since implementation of neighbor lookup is different for lazy vs  greedy. *)
    method virtual private update_node_neighbors_ : ?oldpos:Coord.coordf_t -> Common.nodeid_t -> unit
      (* This method is called each time a node has moved. 
	 Virtual since implementation of neighbor lookup is different for lazy vs  greedy. *)

    (*                                                             *)
    (* The virtual methods which are taurus vs reflecting specifig.*)
    (*                                                             *)
    method virtual boundarize : Coord.coordf_t -> Coord.coordf_t
      (* Documented in worldt.ml *)
    method virtual dist_coords :  Coord.coordf_t -> Coord.coordf_t -> float
      (* Distance between two points. *)
    method virtual are_neighbors : Common.nodeid_t -> Common.nodeid_t -> bool
      (* One-hop neighborhood. *)
    method virtual private neighboring_tiles : int * int -> (int * int) list
      (* Return the 8 neigboring tiles of a tile. *)


    initializer (
      grid_of_nodes_ <- 
      (Array.make_matrix grid_size_x grid_size_y []);
      node_positions_ <- Array.make (Param.get Params.nodes) initial_pos;

      Log.log#log_notice (lazy 
	(sprintf "World: %2f[m]x%2f[m] size,  %2f[m] radio range, %d nodes" 
	  x y rrange (Param.get Params.nodes))
      );
      ignore (tile_size_x)
    )

    (* takes a 'real' position  (ie, in meters) and returns the 
       discrete grid position.
       Note: replicated in crsearch.ml
    *)
    method private pos_in_grid_ (x, y) = (
      assert (x >= 0. && y >= 0. && x <= world_size_x && y <= world_size_y);

      (* the 'min' is in case a point lies on the up/right boundary, in which case
	 the x /. tile_size_x division "fits" and we would get a grid_pos = to 
	 grid_size_x (resp. grid_size_y). *)
      let x_pos = 
	min (grid_size_x - 1) (f2i (floor (x /. tile_size_x)))
      and y_pos =
	min (grid_size_y - 1) (f2i (floor (y /. tile_size_y)))
      in (x_pos, y_pos)
    )
	


    (* Returns a list of all nodes which are in the same tile as pos, or in one of
       the 8 neighboring tiles *)
    method private grid_neighbors_ pos = (
      let tile = s#pos_in_grid_ pos in
      let tiles = s#neighboring_tiles tile in
      List.fold_left 
	(fun l tile -> l @ grid_of_nodes_.(xx tile).(yy tile) )
	[]
	tiles
    )


    method random_pos  = (
      let pos = (Random.float world_size_x, Random.float world_size_y) in
      pos
    )

    method dist_nodeids id1 id2 = s#dist_coords (node_positions_.(id1)) (node_positions_.(id2))

    method private slow_compute_neighbors_ nid = (
      let neighbors = ref [] in
      Nodes.iteri 
	(fun cand_id _ -> if s#are_neighbors nid cand_id  then 
	  neighbors := (cand_id)::!neighbors);
      !neighbors;
    )


    method private compute_neighbors_ nid = 
	List.filter (fun cand_nid -> 
	s#are_neighbors nid cand_nid)  
	(s#grid_neighbors_ node_positions_.(nid))

    method neighbors_consistent = (
      let consistent = ref true in

      (* Check that neighbors are commutative *)
      let commutative() = (
	Nodes.iter 
	(fun n -> 
	  List.iter 
	  (fun ngbr_id -> consistent := !consistent && 
	    List.mem n#id (s#neighbors ngbr_id)
	  )  (s#neighbors n#id)
	);

	!consistent
      ) || raise (Failure "Neighbors not commutative")
      in

      (* Check that all nodes have the correct neigbhors *)
      let correct_neighbors() = (
	Nodes.iter
	(fun n -> 
	  if not (Misc.list_same 
	    (s#neighbors n#id)
	    (s#compute_neighbors_ n#id)) then (
	    Printf.printf "node %d: \n" n#id;
	    List.iter (fun i -> Printf.printf "%d " i) (s#neighbors n#id);
	    Printf.printf "\n";
	    List.iter (fun i -> Printf.printf "%d " i) (s#compute_neighbors_ n#id);
	    Printf.printf "\n";
	  );

	  consistent := !consistent && 
	  (Misc.list_same
	    (s#neighbors n#id)
	    (s#compute_neighbors_ n#id)));
	!consistent
      ) || 
	raise (Failure "Neighbors not correct")
      in

      (* Check that all nodes have the correct neigbhors using slow method *)
      let slow_correct_neighbors() = (
	Nodes.iter
	(fun n -> consistent := !consistent && 
	  (Misc.list_same 
	    (s#neighbors n#id)
	    (s#slow_compute_neighbors_ n#id)));
	!consistent
      ) || raise (Failure "Neighbors not correct (slow_compute_neighbors)")
      in
      commutative()
      &&
      correct_neighbors()
      &&
      slow_correct_neighbors()
    )
      
    method nodepos nid = node_positions_.(nid)

    method movenode ~nid ~newpos  = (
      assert (not (xx newpos < 0.0) || xx newpos > Param.get Params.x_size ||
      yy newpos < 0.0 || yy newpos > Param.get Params.y_size);

      (* update local data structures (grid_of_nodes) with new pos, 
	 then update the node and neighbor node objects *)
      
      let (newx, newy) = s#pos_in_grid_ newpos in
      
      let oldpos = (node_positions_.(nid)) in
      node_positions_.(nid) <- newpos;

      let (oldx, oldy) = (s#pos_in_grid_ oldpos) in
      
      if (oldx, oldy) <> (newx, newy) then (
	(* only update grid_of_nodes if node moved to another slot *)
	
	grid_of_nodes_.(newx).(newy) <- nid::grid_of_nodes_.(newx).(newy);      
	
	assert (List.mem nid (grid_of_nodes_.(oldx).(oldy)));
	grid_of_nodes_.(oldx).(oldy) <- list_without
	  grid_of_nodes_.(oldx).(oldy) nid;      
      );
      s#update_node_neighbors_ ~oldpos nid;

      (* ignore (s#neighbors_consistent || failwith "not consistent");*)

      List.iter 
	(fun mhook -> mhook newpos nid )
	mob_mhooks;
    )

    method add_mob_mhook  ~hook =
      mob_mhooks <- hook::mob_mhooks
	
    method init_pos ~nid ~pos = (
      (* update local data structures (grid_of_nodes) with new pos, 
	 then update the node and neighbor node objects *)

      let (newx, newy) = s#pos_in_grid_ pos in

      if node_positions_.(nid) <> initial_pos then 
	failwith "world.init_pos: node already positioned";
      
      node_positions_.(nid) <- pos;

      (*
	Printf.printf "%s %d %d %d %d \n" 
	(Coord.sprintf pos)
	newx
	newy
	(Array.length grid_of_nodes_)
	(Array.length grid_of_nodes_.(0));
      *)

      assert (not (List.mem nid grid_of_nodes_.(newx).(newy)));
      
      grid_of_nodes_.(newx).(newy) <- nid::grid_of_nodes_.(newx).(newy);      
      
      s#update_node_neighbors_ nid;
      
      List.iter 
	(fun mhook -> mhook pos nid)
	mob_mhooks;
    )
      
    method private is_in_grid p = 
      (xx p) >= 0 && (yy p) >= 0 && 
      ((xx p) < grid_size_x) && ((yy p) < grid_size_y)

    (* Returns nodes in squares that are touched by a ring of unit width. 
       List may have repeated elements.
       radius: outer radius of ring *)
    method private get_nodes_in_ring ~center ~radius = (

      let tiles_at_radius r = (
	let coords = (
	  match r with
	    | rad when (rad <= rrange_) -> 
		let gridpos = s#pos_in_grid_ center in
		s#neighboring_tiles gridpos		
	    | rad  -> 
		(* xxx/ gridsize can be rectangular *)
		Crsearch.xsect_grid_and_circle 
		~center:center 
		~radius:rad
		~world_size_x ~world_size_y
		~tile_size_x ~tile_size_y
		~grid_size_x ~grid_size_y
	) in
	List.filter (fun p -> s#is_in_grid p) coords
      ) in
      
      let inner_squares = tiles_at_radius (radius -. rrange_)
      and outer_squares = tiles_at_radius radius
      in 
      let squares = list_unique_elements (
	inner_squares @ 
	outer_squares
      ) in
      let is_in_ring = (fun n -> 
	((s#dist_coords center node_positions_.(n)) <=  radius) && 
	((s#dist_coords center node_positions_.(n)) >= (radius -. rrange_))) in
      
      List.fold_left (fun l sq -> 
	l @
	(List.filter is_in_ring grid_of_nodes_.(xx sq).(yy sq))
      ) [] squares
    )


    method find_closest ~pos ?(f=(fun _ -> true)) ()= (
      let diagonal_length = 
	(ceil 
	  (sqrt
	    ( 
	      (world_size_x ** 2.0)
	      +.
	      (world_size_y ** 2.0)
	    )
	  ))
      in
      let r = ref rrange in

      let closest = ref None in 

      while (!r <= diagonal_length) && (!closest = None) do
	let candidates = Misc.list_unique_elements
	  (s#get_nodes_in_ring ~center:pos ~radius:!r) in
	let (closest_id, closest_dist) = (ref None, ref max_float) in
	List.iter 
	  (fun nid -> 
	    match f nid with
	      | true ->
		  if (s#dist_coords pos node_positions_.(nid)) < !closest_dist then (
		    closest_id := Some nid;
		    closest_dist := (s#dist_coords pos node_positions_.(nid))
		  )
	      | false -> ()
	  ) candidates;
	closest := !closest_id;
	r := !r +. rrange_
      done;

      (*
	let slow_closest = (s#slow_find_closest ~pos:pos ~f:f) 
	in
	if (!closest <> slow_closest) then (
	if ((s#dist_coords (Nodes.node(o2v !closest))#pos pos) < s#dist_coords
	(Nodes.node(o2v slow_closest))#pos pos) then 
	Printf.printf "We got a closest that is closer!!, radius %f\n" !r
	else if (s#dist_coords (Nodes.node(o2v !closest))#pos pos >
	s#dist_coords (Nodes.node(o2v slow_closest))#pos pos) then 
	Printf.printf "We got a closest that is further,  radius %f\n" !r
	);
	flush stdout;
      *)
      !closest
    )
      
      
    method private slow_find_closest ~pos ~f = (
      let (closest_id, closest_dist) = (ref None, ref max_float) in
      Nodes.iter 
	(fun n -> 
	  match f n with
	    | true ->
		if (s#dist_coords pos node_positions_.(n#id)) < !closest_dist then (
		  closest_id := Some n#id;
		  closest_dist := (s#dist_coords pos node_positions_.(n#id))
		)
	    | false -> ()
	);
      !closest_id
    )


    method get_nodes_within_radius  ~nid ~radius = (
      
      let radius_sq = radius ** 2.0 in
      let center = node_positions_.(nid) in
      let l = ref [] in
      Nodes.iteri (fun cand_id _ -> if s#dist_coords center node_positions_.(cand_id) <= radius then l := (cand_id)::!l);
      !l
    )

    (*  method scale_unit f = f /. gridsize_*)

    method project_2d (x, y) =  (x /. world_size_x, y /. world_size_y)

    method is_connected () = (
      (* Create a graph object reflecting current connectivity *)
      let g = (Graph.make_ 0 (Param.get Params.nodes) Graph.Directed) in
      Nodes.iter (fun n -> Graph.add_node_ g n#id);
      Nodes.iteri (fun i _ -> 
	List.iter (fun ngbr -> Graph.add_edge_ g i ngbr (s#dist_nodeids i ngbr)) 
	(s#neighbors i));
      (* Iterate over all src-dst pairs and check if there is a route *)
      try 
	Graph.itern_ (fun src -> 
	  Graph.itern_ (fun dst -> 
	    ignore (Graph.route_dij_ g src dst);
	  ) g
	) g;

	true
      with 
	| (Failure "No_route") -> false
    )

  end
)


(** Implementation of {!Worldt.lazy_world_t} using a greedy approach to maintaining
  neighbor positions. In other words, a node's neighbors are only re-computed
  each time it moves. This is usually slower than the lazy approach of
  {!Crworld.world_lazy}, but is necessary if using the [add_new_ngbr_hook]
  facility of {!Crworld.world_greedy}.

  @param x X size of World.in meters.
  @param y Y size in meters of World.
  @param rrange Radio range of nodes. Used to determine the coarseness 
  of the discrete grid (quantification) of node positions.
*)
class virtual world_greedy  = (
  object(s)

    val ngbrs = (Array.make (Param.get Params.nodes) ([]:Common.nodeid_t list))
      (* not exposed in virtual class world_ because the way it is
	 maintained up-to-date is different depending on lazy/greedy style *)

    val mutable new_ngbr_hooks = 
      (Array.make (Param.get Params.nodes) ([]:((Common.nodeid_t -> unit) list)))

    method add_new_ngbr_hook nid ~hook =
      new_ngbr_hooks.(nid) <- new_ngbr_hooks.(nid) @ [hook]

    method private update_node_neighbors_ ?oldpos nid = (

      (* 
	 For all neighbors, do a lose_neighbor_ on the neighbor and on this node.
	 Then, compute new neighbors, and add them to this node and to the neighbors.
      *)

      (* not really sure if redefining List.mem here helps performance
	 (intention is to avoid polymorphic = ) *)

      let rec mem (x:int) = function
	  [] -> false
	| a::l -> a = x || mem x l
      in

      let old_neighbors = ngbrs.(nid) in
      let new_neighbors = s#compute_neighbors_ nid in

      let exits = List.filter (fun nid -> not (mem nid new_neighbors)) old_neighbors
      and entries = List.filter (fun nid -> not (mem nid old_neighbors)) new_neighbors

      in
      List.iter 
	(fun i -> 
	  s#lose_neighbor_ ~nid  ~ngbrid:i;
	  s#lose_neighbor_ ~nid:i ~ngbrid:nid
	) exits;

      List.iter 
	(fun i -> 
	  s#add_neighbor_ ~nid ~ngbrid:i;
	  if i <> nid then
	    (* don't add twice for node itself *)
	    s#add_neighbor_ ~nid:i ~ngbrid:nid
	) entries
    )


    (* adds a new neighbor ngbrid to nid. 
       does NOT do the symetric operation *)
    method private add_neighbor_ ~nid ~ngbrid = (
      assert (not (List.mem ngbrid ngbrs.(nid) ));
      List.iter 
	(fun hook -> hook ngbrid)
	new_ngbr_hooks.(nid);

      ngbrs.(nid) <- ngbrid::ngbrs.(nid);
    )

    (* removes neighbor ngbrid from nid. 
       does NOT do the symetric operation *)
    method private lose_neighbor_ ~nid ~ngbrid = (
      assert (List.mem ngbrid ngbrs.(nid) );
      ngbrs.(nid) <- Misc.list_without ngbrs.(nid) ngbrid
    )

    method neighbors nid =  ngbrs.(nid)
  end
)





(** Implementation of {!Worldt.lazy_world_t} using a lazy approach to maintaining
  neighbor positions. In other words, a node's neighbors are only computed
  when needed (for example by when the [#neighbors] method is invoked).
  This is usually faster than the greedy approach of
  {!Worldt.greedy_world_t}, but disallows using the [add_new_ngbr_hook]
  facility of {!Worldt.lazy_world_t}.

  @param x X size of World.in meters.
  @param y Y size in meters of World.
  @param rrange Radio range of nodes. Used to determine the coarseness 
  of the discrete grid (quantification) of node positions.
*)
class virtual world_lazy = (
  object(s)

    val ngbrs = (Array.make (Param.get Params.nodes) ([]:Common.nodeid_t list))
      (* not exposed in virtual class world_ because the way it is
	 maintained up-to-date is different depending on lazy/greedy style *)

    val dirty =  (Array.make (Param.get Params.nodes) true)

    method neighbors nid = (
      if dirty.(nid) then ngbrs.(nid) <- s#compute_neighbors_ nid;
      dirty.(nid) <- false;
      ngbrs.(nid)
    )

    method private update_node_neighbors_ ?oldpos nid = 

      let nodepos = s#nodepos nid in 
      List.iter (fun n -> dirty.(n) <- true) (s#grid_neighbors_ nodepos);
      
      match oldpos with 
	  (* if the node moved across a grid boundary, then potentially nodes
	     from its previous grid_neighbors are dirty as well *)
	| Some p -> 
	    let (newx, newy) = s#pos_in_grid_ nodepos
	    and (oldx, oldy) = s#pos_in_grid_ p in
	    if (oldx, oldy) <> (newx, newy) then 
	      List.iter (fun n -> dirty.(n) <- true) (s#grid_neighbors_ p)
	| None -> ()
	    
  end
)

class virtual reflecting_world ~x ~y ~rrange = (
  object(s)

    inherit world_common ~x ~y ~rrange

    method private neighboring_tiles tile = 
      let is_in_grid p = 
	(xx p) >= 0 && (yy p) >= 0 && 
	((xx p) < grid_size_x) && ((yy p) < grid_size_y) in

      let north = 0,1 and south = 0,-1 and west = -1,0 and east = 1,0 in
      let northeast = north +++ east 
      and northwest = north +++ west
      and southeast = south +++ east
      and southwest = south +++ west in
      List.filter is_in_grid 
	[tile;
	tile +++ north;
	tile +++ east; 
	tile +++ west;
	tile +++ south;
	tile +++ northeast; 
	tile +++ southeast; 
	tile +++ northwest; 
	tile +++ southwest]
	
    method boundarize pos = 
      assert (
	(* make sure the point is within bounds as explained in worldt.ml *)
	(xx pos < (2. *. world_size_x)) &&
	(xx pos > ~-. world_size_x) &&
	(yy pos < (2. *. world_size_y)) &&
	(yy pos > ~-. world_size_y)
      );

      let newx = ref (xx pos) and newy = ref (yy pos) in 
      if !newx >  world_size_x then 
	newx := (2.0 *. world_size_x) -. !newx
      else if !newx < 0.0 then
	newx := (-1.0) *. !newx;
      if !newy > world_size_y  then  
	newy := (2.0 *. world_size_y) -. !newy
      else if !newy < 0.0 then
	newy := (-1.0) *. !newy;

      assert (!newx >= 0.0 && !newx <=  world_size_x && !newy >= 0.0 && !newy <=  world_size_y);
      (!newx, !newy)
      
    method dist_coords a b = sqrt (Coord.dist_sq a b)
    method are_neighbors nid1 nid2 = 
      nid1 <> nid2 && ((Coord.dist_sq (node_positions_.(nid1)) (node_positions_.(nid2))) <= rrange_sq_)

  end
)


class virtual taurus_world ~x ~y ~rrange = (
  object(s)

    inherit world_common ~x ~y ~rrange

    method private neighboring_tiles tile = 
      
      let taur_xx c = match (Coord.xx c) with
	| -1 -> grid_size_x -1
	| x when (x = grid_size_x) -> 0
	| x when (isin x (0, grid_size_x - 1)) -> x
	| _ -> raise (Failure ("Crworld.taurus_world.grid_neighbors_"))
      and taur_yy c = match (Coord.yy c) with
	| -1 -> grid_size_y -1
	| y when (y = grid_size_y) -> 0
	| y when (isin y (0, grid_size_y - 1)) -> y
	| _ -> raise (Failure ("Crworld.taurus_world.grid_neighbors_"))
      in
      let north = 0,1 and south = 0,-1 and west = -1,0 and east = 1,0 in
      let northeast = north +++ east 
      and northwest = north +++ west
      and southeast = south +++ east
      and southwest = south +++ west in
      List.map (fun a_tile -> (taur_xx a_tile, taur_yy a_tile))
	[tile;
	tile +++ north;
	tile +++ east; 
	tile +++ west;
	tile +++ south;
	tile +++ northeast; 
	tile +++ southeast; 
	tile +++ northwest; 
	tile +++ southwest]


    method boundarize pos = 
      assert (
	(* make sure the point is within bounds as explained in worldt.ml *)
	(xx pos < (2. *. world_size_x)) &&
	(xx pos > ~-. world_size_x) &&
	(yy pos < (2. *. world_size_y)) &&
	(yy pos > ~-. world_size_y)
      );

      let newx = ref (xx pos) and newy = ref (yy pos) in 
      if !newx >  world_size_x then 
	newx := !newx -. world_size_x
      else if !newx < 0.0 then
	newx := world_size_x +. !newx;
      if !newy >  world_size_y then 
	newy := !newy -. world_size_y
      else if !newy < 0.0 then
	newy := world_size_y +. !newy;

      assert (!newx >= 0.0 && !newx <=  world_size_x && !newy >= 0.0 && !newy <=  world_size_y);
      (!newx, !newy)

    method private dist_squared a b = (
      
      let dist_axis x1 x2 size = 
	(* computes the distance along one axis, given size along that dimension,
	   and assuming that the range starts at 0 (ie, [0,width]) *)
	let d = x1 -. x2 in
	let abs_d = abs_float d in
	if d < 0. then (* x2 is 'to the right' of x1 *)
	  min abs_d (abs_float (d +. size))
	else (* x2 is 'to the left' of x1 *)
	  min abs_d (abs_float (d -. size))
      in
      ((dist_axis (xx a) (xx b) world_size_x) ** 2.0)
      +.
	((dist_axis (yy a) (yy b) world_size_y) ** 2.0)
    )

    method dist_coords a b = sqrt (s#dist_squared a b)

    method are_neighbors nid1 nid2 = 
      nid1 <> nid2 && ((s#dist_squared (node_positions_.(nid1)) (node_positions_.(nid2))) <= rrange_sq_)

  end
)


class lazy_reflecting_world ~x ~y ~rrange : Worldt.lazy_world_t = (
  object(s)
    inherit world_lazy
    inherit reflecting_world ~x ~y ~rrange
  end
)

class greedy_reflecting_world ~x ~y ~rrange : Worldt.greedy_world_t = (
  object(s)
    inherit world_greedy
    inherit reflecting_world ~x ~y ~rrange
  end
)

class lazy_taurus_world ~x ~y ~rrange : Worldt.lazy_world_t = (
  object(s)
    inherit world_lazy
    inherit taurus_world ~x ~y ~rrange
  end
)

class greedy_taurus_world ~x ~y ~rrange : Worldt.greedy_world_t = (
  object(s)
    inherit world_greedy
    inherit taurus_world ~x ~y ~rrange
  end
)




class epflworld ~x ~y ~rrange = 
object(s)
  inherit lazy_reflecting_world ~x ~y ~rrange
  method random_pos =  
    let nodeind = Random.int 113 in
    Read_coords.box_centeri nodeind
end



