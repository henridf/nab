(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* Continuous topology with reflective boundaries *)

open Coord
open Misc
open Common
open Printf

(* This class duplicates code from  contTaurusTop.ml, keep in mind when 
   changing things *)

class crworld ~node_cnt : World.world_t = 
object(s)

  val mutable grid_of_nodes_ =  (Array.make_matrix 1 1 [])
    
  val mutable gridsize_ =  1.0 (* in practice an int, but stored as float to avoid many i2f's*)
  val mutable center_ =  [|0.5; 0.5|]

  initializer (
    let g = round (sqrt (i2f node_cnt)) in
    gridsize_ <- g;
    center_ <- [|g /. 2.0; g /. 2.0|];
    grid_of_nodes_ <- (Array.make_matrix (f2i g) (f2i g) []);
  )

  method private pos_in_grid_ pos = coord_f2i (coord_floor pos)

  method random_pos  = (
    let pos = [|Random.float gridsize_; Random.float gridsize_|] in
    pos
  )

  method private reflect pos = (
    let newx = ref (x pos) and newy = ref (y pos) in 
    if !newx >  gridsize_ then 
      newx := (2.0 *. gridsize_) -. !newx
    else if !newx < 0.0 then
      newx := (-1.0) *. !newx;
    if !newy > gridsize_  then  
      newy := (2.0 *. gridsize_) -. !newy
    else if !newy < 0.0 then
      newy := (-1.0) *. !newy;
    assert (!newx >= 0.0 && !newx <  gridsize_ && !newy >= 0.0 && !newy <  gridsize_);
    [|!newx; !newy|];
  )

  method boundarize pos = s#reflect pos

  method dist_coords a b = sqrt (Coord.dist_sq a b)
  method dist_nodes n1 n2 = s#dist_coords n1#pos n2#pos
  method dist_nodeids id1 id2 = s#dist_coords (Nodes.node(id1))#pos (Nodes.node(id2))#pos
  method neighbors n1 n2 = (s#dist_coords n1#pos n2#pos) <= 1.0
    
  method private compute_neighbors node = (
    let neighbors = ref NodeSet.empty in
    Nodes.iter 
      (fun a_node -> if s#neighbors node a_node  then 
	neighbors := NodeSet.add a_node#id !neighbors);
    !neighbors
  )

  method neighbors_consistent = (
    let consistent = ref true in

    (* Check that neighbors are commutative *)
    let commutative() = (
      Nodes.iter 
      (fun n -> 
	NodeSet.iter 
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
	(Common.NodeSet.equal n#neighbors (s#compute_neighbors n)));
      !consistent
    ) || raise (Failure "Neighbors not correct")
    in
    commutative()
    &&
    correct_neighbors()
  )
    
  method update_pos ~node ~oldpos_opt = (
    (* 
       For all neighbors, do a lose_neighbor on the neighbor and on this node.
       Then, compute new neighbors, and add them to this node and to the neighbors.
    *) 
    
    let index = node#id in
    let newpos = node#pos in

    let (newx, newy) = ((s#pos_in_grid_ newpos).(0), (s#pos_in_grid_ newpos).(1)) in

    let _ = 
    match oldpos_opt with

      | None ->  grid_of_nodes_.(newx).(newy) <- index::grid_of_nodes_.(newx).(newy);      
	  (* node is new, had no previous position *)

      | Some oldpos -> (
	  let (oldx, oldy) = ((s#pos_in_grid_ oldpos).(0), (s#pos_in_grid_ oldpos).(1)) in

	  if (oldx, oldy) <> (newx, newy) then (
	    (* only update grid_of_nodes if node moved to another slot *)

	    grid_of_nodes_.(newx).(newy) <- index::grid_of_nodes_.(newx).(newy);      
	    assert (List.mem index (grid_of_nodes_.(oldx).(oldy)));
	    grid_of_nodes_.(oldx).(oldy) <- list_without
	      grid_of_nodes_.(oldx).(oldy) index;      
	  )
	) 
    in

    s#update_node_neighbors_ node;
  )
    

  method private update_node_neighbors_ node = (

    let old_neighbors = node#neighbors in
    let new_neighbors = s#compute_neighbors node in
    (* figure out which nodes have entered or exited neighborhood *)
    let changed_neighbors = 
      (NodeSet.union 
	(NodeSet.diff old_neighbors new_neighbors) 
	(NodeSet.diff new_neighbors old_neighbors)) 
    in
    NodeSet.iter 
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

(*
  method update_node_neighbors node = (
    let n = Array.length data.db 
    and ntargets = Array.length data.db.(0) in
    
    (* we do not exploit the fact that the 'meeting' is symetric (if A meets B, B meets A)*)
    
    let _update_db node neighbor = 
      data.db.(node).(neighbor) <- meeting (get_time())  data.pos.(neighbor) in
    
    for i = 0 to n - 1 do
      for j = 0 to ntargets - 1 do
	if (s#zero_hop_neighbors data.pos.(i) data.pos.(j)) then
	  _update_db i j;
      done;
    done;
  )
*)

  method find_closest ~src ~f = (
    let (closest_id, closest_dist) = (ref 0, ref max_float) in
    Nodes.iter 
      (fun n -> 
	match f n with
	  | true ->
	      if (s#dist_coords src#pos n#pos) < !closest_dist then (
		closest_id := n#id;
		closest_dist := (s#dist_coords src#pos n#pos)
	      )
	  | false -> ()
      );
    !closest_id
  )

  method get_nodes_within_radius  ~node ~radius = (
    
    let radius_sq = radius ** 2.0 in
    let center = node#pos in
    let l = ref [] in
    Nodes.iter (fun node -> if s#dist_coords center node#pos <= radius then l := (node#id)::!l) ;
    !l
  ) 

  method scale_unit f = f /. gridsize_

  method project_2d coord =  Array.map (fun x -> s#scale_unit x) coord 

  method get_nodes_at pos = 
    let scaleup = pos ***. gridsize_ in
    let (x,y) = ((s#pos_in_grid_ scaleup).(0), (s#pos_in_grid_ scaleup).(1)) in
    grid_of_nodes_.(x).(y)

  method sprint_info () = Printf.sprintf "\tGridsize:\t\t\t %f\n" gridsize_

end


