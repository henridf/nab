(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Continuous topology with reflective boundaries                                      *)

open Coord
open Misc
open Common


module ContRefTop : Top.Topology_t = 
  (* This module duplicates code from  contTaurusTop.ml, keep in mind when 
   changing things *)
  struct 

    type grid_of_nodes_t = int list array array ref
    let grid_of_nodes_ = (ref (Array.make_matrix 1 1 []):grid_of_nodes_t)

    let gridsize_ = ref 1.0 (* in practice an int, but stored as float to avoid many i2f's*)
    let center_ = ref [|0.5; 0.5|]

    (* setup internal structures *)
    let initialize () = (
      gridsize_ := round (sqrt (i2f (Param.get Params.nodes)));
      center_ := [|!gridsize_ /. 2.0; !gridsize_ /. 2.0|];
      grid_of_nodes_ := (Array.make_matrix (f2i !gridsize_) (f2i !gridsize_) [])
    )

    let pos_in_grid_ pos = coord_f2i (coord_floor pos)

    (* return an initial pos for node i *)
    let initial_pos i = (
      let pos = [|Random.float !gridsize_; Random.float !gridsize_|] in
      let (x, y) = ((pos_in_grid_ pos).(0), (pos_in_grid_ pos).(1)) in
      pos
    )

    (* set position internally *)
    let place_node i pos = (
      let (x, y) = ((pos_in_grid_ pos).(0), (pos_in_grid_ pos).(1)) in
      assert (not (List.mem i !grid_of_nodes_.(x).(y)));
      !grid_of_nodes_.(x).(y) <- i::!grid_of_nodes_.(x).(y);
    )

    let reflect_ pos = (
      let newx = ref (x pos) and newy = ref (y pos) in 
      if !newx >  !gridsize_ then 
	newx := (2.0 *. !gridsize_) -. !newx
      else if !newx < 0.0 then
	newx := (-1.0) *. !newx;
      if !newy > !gridsize_  then  
	newy := (2.0 *. !gridsize_) -. !newy
      else if !newy < 0.0 then
	newy := (-1.0) *. !newy;
      assert (!newx >= 0.0 && !newx <  !gridsize_ && !newy >= 0.0 && !newy <  !gridsize_);
      [|!newx; !newy|];
    )


    let dist a b = sqrt (Coord.dist_sq a b)
    let zero_hop_neigbors pos1 pos2 = (coord_round pos1) = (coord_round pos2)

    let (waypoint_move, waypoint_init) = (

      let init_done = ref false in
      let waypoint_targets = ref (Array.make 1 ([|0.0; 0.0|]:coordf_t)) in
      
      let init () = (
	waypoint_targets := (Array.make (Param.get Params.nodes) ([|0.0; 0.0|]:coordf_t));
	Array.iteri (
	  fun i nothing  -> 
	    !waypoint_targets.(i) <- [|Random.float !gridsize_; Random.float !gridsize_|]
	) !waypoint_targets;
      ) in
	
      let  move data ~index  =  (

	if (!init_done = false)  then (
	  (* need to setup waypoint targets the very first time *)
	  init();
	  init_done := true;
	);

      	let target = !waypoint_targets.(index) in
	let pos = data.pos.(index) in
	if (dist target pos) <= 1.0 then 
	  (* arrived at target *)
	  begin
	    !waypoint_targets.(index) <- [|Random.float !gridsize_; Random.float !gridsize_|];
	    target
	  end
	else 
	  begin
	    let direction = normalize (target ---. pos) in
	    pos +++. direction;
	  end
      )

      in
      (move, init)
    )

    let next_position data ~index ~mob = (
      match mob with
	| RANDOMWALK -> 
	    reflect_ (
	      data.pos.(index) +++. ([|Random.float 2.0; Random.float 2.0|] ---. [|1.0; 1.0|])
	    )
	| WAYPOINT -> waypoint_move data ~index:index
    )

    let update_pos ~index ~oldpos ~newpos = (
      let (oldx, oldy) = ((pos_in_grid_ oldpos).(0), (pos_in_grid_ oldpos).(1)) in
      let (newx, newy) = ((pos_in_grid_ newpos).(0), (pos_in_grid_ newpos).(1)) in
      assert (List.mem index (!grid_of_nodes_.(oldx).(oldy)));
      !grid_of_nodes_.(oldx).(oldy) <- list_without !grid_of_nodes_.(oldx).(oldy) index;      
      !grid_of_nodes_.(newx).(newy) <- index::!grid_of_nodes_.(newx).(newy);      
    )
     
    let get_nodes_within_radius data ~node ~radius = (
      
      let radius_sq = radius ** 2.0 in
      let center = data.pos.(node) in
      let l = ref [] in
      Array.iteri (fun node pos -> if dist center pos <= radius then l := node::!l) data.pos;
      !l
    ) 

    (* for now, route in continuous domain is direct path *)
    let route data ~src ~dst = [data.pos.(src)]

    let scale_unit f = f /. !gridsize_

    let project_2d coord =  Array.map (fun x -> scale_unit x) coord 

    let get_nodes_at pos = 
      let scaleup = pos ***. !gridsize_ in
      let (x,y) = ((pos_in_grid_ scaleup).(0), (pos_in_grid_ scaleup).(1)) in
      !grid_of_nodes_.(x).(y)

    let sprint_info () = Printf.sprintf "\tGridsize:\t\t\t %f\n" !gridsize_

end


