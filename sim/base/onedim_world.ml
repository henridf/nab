(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)

open Misc
open Common
open Printf

(* some methods are replicated from crworld. so we could start to make a base
   class from which we inherit those methods... but the common code is not
   huuuge so for now leaving as is. *)


(** 
  Model of a one-dimensional world. We still use two dimensional coordinates
  ({!Coord.coordf_t}) for sane type-compatibility with everything else, but
  only use the first  component.

  @author Henri Dubois-Ferriere.
*)
class virtual onedim_world_common  ~x ~rrange = (
  object(s)
    
    val mutable grid_of_nodes_ =  (Array.make 1 ([]:Common.nodeid_t list)) 
    val mutable node_positions_ =  [|0.|]
      
    method private grid_of_nodes = grid_of_nodes_
      
    val world_size =  x 
    val rrange_ = rrange
      
    (* Nodes inhabit a line segment.
       We divide it into smaller segments ("tiles" by abuse of notation from
       the 2d case) of size equal to radio range. 
    *)
    val grid_size = f2i (floor (x /. rrange));
    val tile_size = x /. (floor (x /. rrange));
    
    val mutable mob_mhooks = []
      
    val initial_pos = 0.0

    (*                                                        *)
    (* The virtual methods which are lazy vs greedy specific. *)
    (*                                                        *)
    method virtual neighbors : Common.nodeid_t -> Common.nodeid_t list
      (* See {!Worldt.lazy_world_t.neighbors}.
	 Virtual since implementation of neighbor lookup is different for lazy vs  greedy. *)
    method virtual private update_node_neighbors_ : ?oldpos:float -> Common.nodeid_t -> unit
      (* This method is called each time a node has moved. 
	 Virtual since implementation of neighbor lookup is different for lazy vs  greedy. *)

    (*                                                             *)
    (* The virtual methods which are taurus vs reflecting specifig.*)
    (*                                                             *)
    method virtual boundarize : Coord.coordf_t -> Coord.coordf_t
      (* Documented in worldt.ml *)
    method virtual private dist_coords_ :  float -> float -> float
      (* Distance between two points. *)
    method virtual are_neighbors : Common.nodeid_t -> Common.nodeid_t -> bool
      (* One-hop neighborhood. *)
    method virtual private neighboring_tiles : int -> int list
      (* Return the (one or two) neigboring tiles of a tile. *)

    initializer (
      grid_of_nodes_ <- (Array.make grid_size []);
      node_positions_ <- Array.make (Param.get Params.nodes) initial_pos;

      Log.log#log_notice (lazy 
	(sprintf "1D World: %.2f [m],  %.2f radio range [m], %d nodes" 
	  x rrange (Param.get Params.nodes))
      );
    )

    method dist_coords (x1, _) (x2, _) = s#dist_coords_ x1 x2

    (* takes a 'real' position  (ie, in meters) and returns the 
       discrete grid position.  *)
    method private pos_in_grid_ x = (
      assert (x >= 0. && x <= world_size);
      
      (* the 'min' is in case a point lies on the up/right boundary, in which case
	 the x /. tile_size_x division "fits" and we would get a grid_pos = to 
	 grid_size_x (resp. grid_size_y). *)
      min (grid_size - 1) (f2i (floor (x /. tile_size)))
    )

    (* Returns a list of all nodes which are in the same tile as pos, or in
       one of the 2 neighboring tiles *)
    method private grid_neighbors_ x = (
      let tile = s#pos_in_grid_ x in
      let tiles = s#neighboring_tiles tile in
      List.fold_left 
	(fun l tile -> l @ grid_of_nodes_.(tile))
	[]
	tiles
    )


    method random_pos =  (Random.float world_size, 0.)
      
    (* replicated from crworld*)
    method dist_nodeids id1 id2 = 
      s#dist_coords (node_positions_.(id1), 0.) (node_positions_.(id2), 0.)

    (* replicated from crworld*)
    method private compute_neighbors_ nid = 
      List.filter (fun cand_nid -> 
	s#are_neighbors nid cand_nid)  
	(s#grid_neighbors_ node_positions_.(nid))

    (* replicated (in part) from crworld *)
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

      let single_dim() = 
	(Nodes.fold (fun n b -> b && (Coord.yy (s#nodepos n#id) = 0.0)) true) 
	|| raise (Failure "Nodes in two dimensions!")
      in

      commutative()
      &&
      correct_neighbors()
      &&
      single_dim()
    )

    method nodepos nid = node_positions_.(nid), 0.

    method movenode ~nid ~newpos:(newx, newy)  = (
      assert (not (newx < 0.0) || newx > Param.get Params.x_size);
      assert (newy = 0.0);

      (* update local data structures (grid_of_nodes) with new pos, 
	 then update the node and neighbor node objects *)
      let newgrid = s#pos_in_grid_ newx in
      
      node_positions_.(nid) <- newx;

      let oldx = (node_positions_.(nid)) in
      let oldgrid = (s#pos_in_grid_ oldx) in
      
      if oldgrid <> newgrid then (
	(* only update grid_of_nodes if node moved to another slot *)
	
	grid_of_nodes_.(newgrid) <- nid::grid_of_nodes_.(newgrid);      
	
	assert (List.mem nid (grid_of_nodes_.(oldgrid)));
	grid_of_nodes_.(oldgrid) <- (list_without grid_of_nodes_.(oldgrid) nid);
      );
      
      s#update_node_neighbors_ ~oldpos:oldx nid;
      
      (* ignore (s#neighbors_consistent || failwith "not consistent");*)
      
      List.iter 
	(fun mhook -> mhook (newx, newy) nid )
	mob_mhooks;
    )
      
    (* replicated from crworld*)
    method add_mob_mhook  ~hook =
      mob_mhooks <- hook::mob_mhooks
	

    method init_pos ~nid ~pos = (
      assert (Coord.yy pos = 0.);
      (* update local data structures (grid_of_nodes) with new pos, 
	 then update the node and neighbor node objects *)

      let newx = s#pos_in_grid_ (Coord.xx pos) in

      if node_positions_.(nid) <> initial_pos then 
	failwith "world.init_pos: node already positioned";
      
      node_positions_.(nid) <- (Coord.xx pos);

      assert (not (List.mem nid grid_of_nodes_.(newx)));
      
      grid_of_nodes_.(newx) <- nid::grid_of_nodes_.(newx);      
      
      s#update_node_neighbors_ nid;
      
      List.iter 
	(fun mhook -> mhook pos nid)
	mob_mhooks;
    )

    method private is_in_grid x = x >= 0 &&  x < grid_size
      
    method find_closest ~pos:((posx, _ ) : float * float ) ?(f=(fun _ -> true)) ()= (
      let c_id, c_dist = ref None , ref max_float in 
      let p = s#pos_in_grid_ posx in
      
      let check_node nid = 
	if f nid && (s#dist_coords_ posx node_positions_.(nid)) < !c_dist
	then c_id := Some nid; c_dist := (s#dist_coords_ posx node_positions_.(nid))
	  
      in


      let left, right = ref (p - 1), ref (p + 1) in

      begin try 

	(* do first tile right around 'pos'*)
	List.iter check_node grid_of_nodes_.(p);
	if !c_id <> None then raise Exit;
	
	(* now do expanding search, taking two tiles on either side of pos, at
	   increasing distances. *)
	while !left >= 0 || !right < grid_size do
	  if !left >= 0 then (
	    List.iter check_node grid_of_nodes_.(!left);
	    decr left
	  );
	  if !right < Array.length grid_of_nodes_  then (
	    List.iter check_node grid_of_nodes_.(!right);
	    incr right;
	  );
	  if !c_id <> None then raise Exit;
	done

      with Exit -> ();
      end;
      
      !c_id
    )

    method project_2d ((x, _): float * float ) =  (x /. world_size, 0.5)

    method is_connected() = raise Not_Implemented; true

    method get_nodes_within_radius  ~nid ~radius = 
      let center = node_positions_.(nid) in
      let l = ref [] in
      Nodes.iteri (fun cand_id _ -> if s#dist_coords_ center node_positions_.(cand_id) <= radius then l := (cand_id)::!l);
      !l
	
  end
)

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

      let (nodepos, _) = s#nodepos nid in 
      List.iter (fun n -> dirty.(n) <- true) (s#grid_neighbors_ nodepos);
      
      match oldpos with 
	  (* if the node moved across a grid boundary, then potentially nodes
	     from its previous grid_neighbors are dirty as well *)
	| Some p -> 
	    let newx = s#pos_in_grid_ nodepos
	    and oldx = s#pos_in_grid_ p in
	    if oldx <> newx then 
	      List.iter (fun n -> dirty.(n) <- true) (s#grid_neighbors_ p)
	| None -> ()
	    
  end
)


class virtual onedim_reflecting_world ~x ~rrange = (
  object(s)

    inherit onedim_world_common ~x ~rrange

    method private neighboring_tiles tile = 
      let is_in_grid x = x >= 0 &&  x < grid_size in

    List.filter is_in_grid  [tile; tile + 1; tile - 1]
	
    method boundarize (x, y) = 
      assert (y = 0.);
      assert (
	(* make sure the point is within bounds as explained in worldt.ml *)
	(x < (2. *. world_size)) &&
	(x > ~-. world_size) 
      );

      let newx = ref x in
      if !newx >  world_size then 
	newx := (2.0 *. world_size) -. !newx
      else if !newx < 0.0 then
	newx := -.(!newx);

      assert (!newx >= 0.0 && !newx <=  world_size);
      (!newx, 0.)
      
    method private dist_coords_ a b = abs_float (a -. b)
    method are_neighbors nid1 nid2 = 
      nid1 <> nid2 && abs_float (node_positions_.(nid1) -. node_positions_.(nid2)) <= rrange

  end
)



class virtual onedim_ring_world ~x ~rrange = (
  object(s)

    inherit onedim_world_common ~x ~rrange

    method private neighboring_tiles tile = 
      
      match grid_size > 1 with
	| true -> begin
	    match tile = 0, tile = grid_size with
	      |	false, false -> [tile; tile + 1; tile - 1]
	      | true, false -> [tile; grid_size - 1; tile + 1]
	      | false, true -> [tile; tile - 1; 0]
	      | _ -> raise (Misc.Impossible_Case "Onedim_ring_world.neighboring_tiles")
	  end;
	| false -> [tile]

    method boundarize (x, y) = 
      assert (y = 0.);
      assert (
	(* make sure the point is within bounds as explained in worldt.ml *)
	(x < (2. *. world_size)) &&
	(x > ~-. world_size) 
      );

      let newx = ref x in 
      if !newx >  world_size then 
	newx := !newx -. world_size
      else if !newx < 0.0 then
	newx := world_size +. !newx;

      assert (!newx >= 0.0 && !newx <=  world_size);
      (!newx, 0.)
      
    method private dist_coords_ a b = 
      match a < b with 
	| true -> min (b -. a) (a +. (world_size -. b))
	| false -> min (a -. b) (b +. (world_size -. a))
	    


    method are_neighbors nid1 nid2 = 
      nid1 <> nid2 && s#dist_coords_ node_positions_.(nid1) node_positions_.(nid2) <= rrange
  end
)

class lazy_reflecting_world ~x ~rrange : Worldt.lazy_world_t = (
  object(s)
    inherit world_lazy
    inherit onedim_reflecting_world ~x ~rrange
  end
)


class greedy_reflecting_world ~x ~rrange : Worldt.greedy_world_t = (
  object(s)
    inherit Crworld.world_greedy
    inherit onedim_reflecting_world ~x ~rrange
  end
)


class lazy_taurus_world ~x ~rrange : Worldt.lazy_world_t = (
  object(s)
    inherit world_lazy
    inherit onedim_ring_world ~x ~rrange
  end
)

class greedy_taurus_world ~x ~rrange : Worldt.greedy_world_t = (
  object(s)
    inherit Crworld.world_greedy
    inherit onedim_ring_world ~x ~rrange
  end
)





