(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(* Continuous topology with reflective boundaries *)

open Coord
open Misc
open Common
open Printf



(* x, y : size in meters of world.
   rrange: radio range of nodes (not flexible for now, see
   general_todo.txt) . This value is to determine the coarseness 
   of the discrete grid (quantification) of node positions.
*)

class crworld ~x ~y ~rrange : World.world_t = 
object(s)

  val mutable grid_of_nodes_ =  (Array.make_matrix 1 1 [])
    
  val mutable size_x_ =  x 
  val mutable size_y_ =  y
  val mutable rrange_ = rrange

  initializer (
    grid_of_nodes_ <- (Array.make_matrix 
      (f2i (size_x_ /. rrange_)) 
      (f2i (size_y_ /. rrange_)) 
      []);
    Log.log#log_always (sprintf "New CRWorld " );
  )

  (* takes a 'real' position  (ie, in meters) and returns the 
     discrete grid position *)
  method private pos_in_grid_ pos = coord_f2i (coord_floor (pos ///. rrange_))

  method random_pos  = (
    let pos = (Random.float size_x_, Random.float size_y_) in
    pos
  )

  method private reflect pos = (
    let newx = ref (xx pos) and newy = ref (yy pos) in 
    if !newx >  size_x_ then 
      newx := (2.0 *. size_x_) -. !newx
    else if !newx < 0.0 then
      newx := (-1.0) *. size_x_;
    if !newy > size_y_  then  
      newy := (2.0 *. size_y_) -. !newy
    else if !newy < 0.0 then
      newy := (-1.0) *. !newy;
    assert (!newx >= 0.0 && !newx <  size_x_ && !newy >= 0.0 && !newy <  size_y_);
    (!newx, !newy)
  )

  method boundarize pos = s#reflect pos

  method dist_coords a b = sqrt (Coord.dist_sq a b)
  method dist_nodes n1 n2 = s#dist_coords n1#pos n2#pos
  method dist_nodeids id1 id2 = s#dist_coords (Nodes.node(id1))#pos (Nodes.node(id2))#pos
  method neighbors n1 n2 = n1 <> n2 && (s#dist_coords n1#pos n2#pos) <= 1.0

    
  method private slow_compute_neighbors_ node = (
    let neighbors = ref [] in
    Nodes.iter 
      (fun a_node -> if s#neighbors node a_node  then 
	neighbors := (a_node#id)::!neighbors);
    !neighbors;
  )

  method private compute_neighbors_ node = (
    let gridpos = s#pos_in_grid_ node#pos in
    let grid_at_pos p = 
      if (xx p) >= 0 && (yy p) >= 0 && 
	(xx p) < (f2i (size_x_ /. rrange_)) && (yy p) < (f2i (size_y_ /. rrange_))
      then grid_of_nodes_.(xx p).(yy p) else [] 
    in
    let north = 0,1
    and south = 0,-1 
    and west = -1,0 
    and east = 1,0 in
    let northeast = north +++ east 
    and northwest = north +++ west
    and southeast = south +++ east
    and southwest = south +++ west in
    
    let candidates = 
      grid_at_pos gridpos @
      grid_at_pos (gridpos +++ north) @
      grid_at_pos (gridpos +++ east) @
      grid_at_pos (gridpos +++ west) @
      grid_at_pos (gridpos +++ south) @
      grid_at_pos (gridpos +++ northeast) @
      grid_at_pos (gridpos +++ southeast) @
      grid_at_pos (gridpos +++ northwest) @
      grid_at_pos (gridpos +++ southwest)
    in
    List.filter (fun a_node -> s#neighbors node (Nodes.node(a_node))) candidates
  )

  method neighbors_consistent = (
    let consistent = ref true in

    (* Check that neighbors are commutative *)
    let commutative() = (
      Nodes.iter 
      (fun n -> 
	List.iter 
	(fun n_id -> consistent := !consistent && 
	  ((Nodes.node(n_id))#is_neighbor n))
	n#neighbors
      );
      !consistent
    ) || raise (Failure "Neighbors not commutative")
    in

    (* Check that all nodes have the correct neigbhors *)
    let correct_neighbors() = (
    Nodes.iter
      (fun n -> consistent := !consistent && 
	(Misc.list_same 
	  n#neighbors
	  (s#compute_neighbors_ n)));
      !consistent
    ) || raise (Failure "Neighbors not correct")
    in
    commutative()
    &&
    correct_neighbors()
  )
    
  method update_pos ~node ~oldpos_opt = (
    (* update local data structures (grid_of_nodes) with new pos, 
       then update the node and neighbor node objects *)
    
    let index = node#id in
    let newpos = node#pos in

    let (newx, newy) = s#pos_in_grid_ newpos in

    begin match oldpos_opt with
	
      | None ->  (
	  assert (not (List.mem index grid_of_nodes_.(newx).(newy)));
	  grid_of_nodes_.(newx).(newy) <- index::grid_of_nodes_.(newx).(newy);      
	  (* node is new, had no previous position *)
	)
      | Some oldpos -> (
	  let (oldx, oldy) = (s#pos_in_grid_ oldpos) in
	  
	  if (oldx, oldy) <> (newx, newy) then (
	    (* only update grid_of_nodes if node moved to another slot *)
	    
	    grid_of_nodes_.(newx).(newy) <- index::grid_of_nodes_.(newx).(newy);      
	    assert (List.mem index (grid_of_nodes_.(oldx).(oldy)));
	    grid_of_nodes_.(oldx).(oldy) <- list_without
	      grid_of_nodes_.(oldx).(oldy) index;      
	  )
	)
    end;
    s#update_node_neighbors_ node;
  )
    

  method private update_node_neighbors_ node = (

    (* 
       For all neighbors, do a lose_neighbor on the neighbor and on this node.
       Then, compute new neighbors, and add them to this node and to the neighbors.
    *)

    let old_neighbors = node#neighbors in
    let new_neighbors = s#compute_neighbors_ node in
    let old_and_new = old_neighbors @ new_neighbors in

    let changed_neighbors = 
    (* nodes which have changed status (entered or exited neighborhood)
       are those which are in only one of old_neighbors and new_neighbors *)
      List.fold_left (fun l n -> 
	if ((Misc.list_count_element ~l:old_and_new ~el:n) = 1) then
	  n::l 
	else 
	  l) [] old_and_new 
    in
    List.iter 
      (fun i -> 

	if node#is_neighbor (Nodes.node i) then ( (* these ones left *)
	  node#lose_neighbor (Nodes.node i);
	  (Nodes.node i)#lose_neighbor node

	) else (	  (* these ones entered *)

	  node#add_neighbor (Nodes.node i);
	  if i <> node#id then
	    (* don't add twice for node itself *)
	    (Nodes.node i)#add_neighbor node
	)
      ) changed_neighbors
  )


    (* Returns nodes in squares that are touched by a ring of unit width. 

       List may have repeated elements.
       radius: outer radius of ring *)

  method private get_nodes_in_ring ~center ~radius = (

    let is_in_grid p = 
      (xx p) >= 0 && (yy p) >= 0 && 
      (xx p) < (f2i (size_x_ /. rrange_)) && (yy p) < (f2i (size_y_ /. rrange_))
      in
    let grid_squares_at_radius r = (
	let coords = (
	  match r with
	    | 0 -> []
	    | 1 -> 
		let gridpos = s#pos_in_grid_ center in
		
		let north = 0,1
		and south = 0,-1 
		and west = -1,0 
		and east = 1,0 in
		let northeast = north +++ east 
		and northwest = north +++ west
		and southeast = south +++ east
		and southwest = south +++ west in
		
		[gridpos;
		(gridpos +++ north);
		(gridpos +++ east);
		(gridpos +++ west);
		(gridpos +++ south);
		(gridpos +++ northeast);
		(gridpos +++ southeast);
		(gridpos +++ northwest);
		(gridpos +++ southwest)
		]
	    | r  -> 
		Crsearch.xsect_grid_and_circle ~center:center ~radius:(i2f r) ~gridsize:size_x_
	) in
	List.filter (fun p -> is_in_grid p) coords
      ) in
    
      let inner_squares = grid_squares_at_radius (radius - 1)
      and outer_squares = grid_squares_at_radius radius
      in 
      let squares = list_unique_elements (
	inner_squares @ 
	outer_squares
      ) in
      let is_in_ring = (fun n -> 
	((s#dist_coords center (Nodes.node(n))#pos) <= (i2f radius)) && 
	((s#dist_coords center (Nodes.node(n))#pos) >= (i2f (radius - 1)))) in
      
      List.fold_left (fun l sq -> 
	l @
	(List.filter is_in_ring grid_of_nodes_.(xx sq).(yy sq))
      ) [] squares
  )


  method find_closest ~pos ~f = (
    let diagonal_length = f2i (ceil (sqrt (2.0 *. (size_x_ *. size_y_)))) in
    let i = ref 1 in

    let closest = ref None in 

    while (!i <= diagonal_length) && (!closest = None) do
      let candidates = Misc.list_unique_elements
	(s#get_nodes_in_ring ~center:pos ~radius:!i) in
      let (closest_id, closest_dist) = (ref None, ref max_float) in
      List.iter 
	(fun nid -> 
	  let n = Nodes.node(nid) in
	  match f n with
	    | true ->
		if (s#dist_coords pos n#pos) < !closest_dist then (
		  closest_id := Some n#id;
		  closest_dist := (s#dist_coords pos n#pos)
		)
	    | false -> ()
	) candidates;
      closest := !closest_id;
      incr i
    done;

(*
    let slow_closest = (s#slow_find_closest ~pos:pos ~f:f) 
    in
    if (!closest <> slow_closest) then (
      if ((s#dist_coords (Nodes.node(o2v !closest))#pos pos) < s#dist_coords
	(Nodes.node(o2v slow_closest))#pos pos) then 
	Printf.printf "We got a closest that is closer!!, radius %d\n" (!i - 1)
      else if (s#dist_coords (Nodes.node(o2v !closest))#pos pos >
      s#dist_coords (Nodes.node(o2v slow_closest))#pos pos) then 
	Printf.printf "We got a closest that is further, radius %d\n" (!i - 1);
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
	      if (s#dist_coords pos n#pos) < !closest_dist then (
		closest_id := Some n#id;
		closest_dist := (s#dist_coords pos n#pos)
	      )
	  | false -> ()
      );
    !closest_id
  )

  method get_nodes_within_radius  ~node ~radius = (
    
    let radius_sq = radius ** 2.0 in
    let center = node#pos in
    let l = ref [] in
    Nodes.iter (fun node -> if s#dist_coords center node#pos <= radius then l := (node#id)::!l);
    !l
  )

(*  method scale_unit f = f /. gridsize_*)

  method project_2d (x, y) =  (x /. size_x_, y /. size_y_)

  method get_node_at ~unitpos = 
    let (x_unit, y_unit) = unitpos in
    let scaleup = (x_unit *. size_x_, y_unit *. size_y_) in
    let (x,y) = (s#pos_in_grid_ scaleup) in
    o2v (s#find_closest ~pos:scaleup ~f:(fun _ -> true))


end


