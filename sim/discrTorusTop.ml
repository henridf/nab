(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Discrete square torus topology                                        *)

open Misc
open Coord
open Graph
open Common

module DiscrTorusTop : Top.Topology_t = 
  (* This module duplicates code from  discrRefTop.ml, keep in mind when 
   changing things *)
  struct

    let g = ref (Graph.make_wrap_lattice_ ~dim:2 ~side:4)
    let gridsize_ = ref 1
    let center_ = ref [|1; 1|]

    (* Functions that implement the signature *)
    let initialize () = (
      gridsize_ := f2i (round (sqrt (i2f (Param.get Params.nodes))));
      center_ := [| !gridsize_ / 2; !gridsize_/2|];
      g := Graph.make_wrap_lattice_ ~side:!gridsize_ ~dim:2;
    )

    let initial_pos i = (
      let pos = [|Random.int !gridsize_; Random.int !gridsize_|] in
      coord_i2f pos
    )

    let place_node i pos = (
      let posi = coord_f2i pos in
      assert (not (List.mem i (Graph.getinfo_ !g posi)));
      Graph.appendinfo_ !g posi i;
    )

    let get_nodes_within_radius data ~node ~radius = (
      let neigbor_points = Graph.nhop_and_less_neigbors_ !g ~node:(coord_f2i data.pos.(node)) ~radius:(f2i radius) in
      List.concat (List.map (fun x -> Graph.getinfo_ !g x) neigbor_points)
    )
      
    let dist a b = 
      i2f (Graph.lattice_dist_ !g ~src:(coord_f2i a) ~dest:(coord_f2i b))
	
    let zero_hop_neigbors coord1 coord2 = (coord1 = coord2)
      
  let (waypoint_move, waypoint_init) = (

    let init_done = ref false in
    let waypoint_targets = ref (Array.make 1 ([|0;0|]:coordi_t)) in
    
    let init () = (
      waypoint_targets := (Array.make params.n ([|0;0|]:coordi_t));
      Array.iteri (
	fun i nothing  -> 
	  !waypoint_targets.(i) <- [|Random.int !gridsize_; Random.int !gridsize_|]
      ) !waypoint_targets;
    ) in
    
    let move data ~index  =  (

	if (!init_done = false)  then (
	  (* need to setup waypoint targets the very first time *)
	  init();
	  init_done := true;
	);

      let target = !waypoint_targets.(index) in
      let pos = data.pos.(index) in
      if (dist_sq_reflect target pos) <= 1 then 
	(* arrived at target *)
	begin
	  !waypoint_targets.(index) <- [|Random.int !gridsize_; Random.int !gridsize_|];
	  if index = 0 then (
	    Ler_graphics.circle_nodes [|!waypoint_targets.(index)|] 0.3;
	    Ler_graphics.draw_nodes [|target|];
	  );
	  target
	end
      else 
	begin
	  let direction = normalize (coord_i2f (target --- pos)) in
	  let increment =
	    match coordf2pair (coord_round direction) with
	      | (1.0, 1.0) -> rnd_from_list [[|1; 0|]; [|0;1|]] 
	      | (1.0, -1.0) -> rnd_from_list [[|1; 0|]; [|0;-1|]] 
	      | (-1.0, 1.0) -> rnd_from_list [[|-1; 0|]; [|0;1|]] 
	      | (-1.0, -1.0) -> rnd_from_list [[|-1; 0|]; [|0;-1|]] 
	      | default -> coord_f2i (coord_round direction) 
	  in
	  if index = 0 then Ler_graphics.draw_nodes [|pos +++ increment|];
	  pos +++ increment;
	end
    )
      


  in
  (move, init)
)

    let next_position data ~index ~mob = (
      match mob with
	| RANDOMWALK -> 
	    coord_i2f (rnd_from_list (Graph.neigbors_lattice_ !g (coord_f2i data.pos.(index)) ~side:!gridsize_))
	| WAYPOINT -> (
	    Printf.printf "!*!\nWaypoint model has never been tested in this
      topology. look at the node movement  and make sure it looks ok!\n!*!\n"
	    raise waypoint_move data ~index:index
	  )
    )

    let update_pos ~index ~oldpos ~newpos = (
      assert (List.mem index (Graph.getinfo_ !g (coord_f2i oldpos)));
      Graph.setinfo_ !g (coord_f2i oldpos) (list_without (Graph.getinfo_ !g (coord_f2i oldpos)) index);
      Graph.appendinfo_ !g (coord_f2i newpos) index;
    )

    let route data ~src ~dst = 
      (* we can do the coord_f2i because we know that in this topology all 
	 float coords are integer valued *)
      List.map 
	(fun icoord -> Coord.coord_i2f icoord)
	(Graph.route_dij_ !g 
	  (Coord.coord_f2i data.pos.(src)) 
	  (Coord.coord_f2i data.pos.(dst)))

    let scale_unit f = f /. (i2f !gridsize_)

    let project_2d coord =  Array.map (fun x -> scale_unit x) coord 

    let get_nodes_at place = Graph.getinfo_ !g (coord_f2i (coord_round (place ***. (i2f !gridsize_))))

    let sprint_info () = Printf.sprintf "\tGridsize:\t\t\t %d\n" !gridsize_

  end
  



