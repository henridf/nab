(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Discrete square torus topology                                        *)

open Misc
open Coord
open Graph
open Common

class discrTorusTop : World.world = 
  object(s)

  val mutable graph = Graph.make_wrap_lattice_ ~dim:2 ~side:4
  val mutable gridsize_ =  1
  val mutable center_ = [|1; 1|]
  val mutable waypoint_targets_ = (Array.make 1 [|0.0; 0.0|])

  initializer (
    let g = f2i (round (sqrt (i2f (Param.get Params.nodes)))) in
    gridsize_ <- g;
    center_ <- [| gridsize_ / 2; gridsize_/2|];
    graph <- Graph.make_wrap_lattice_ ~side:gridsize_ ~dim:2;
    waypoint_targets_ <- (
      let w = ref (Array.make (Param.get Params.nodes) ([|0.0; 0.0|]:coordf_t)) in
      Array.iteri (
	fun i nothing  -> 
	  !w.(i) <- coord_i2f [|Random.int g; Random.int g|]
      ) !w;
      !w
    )
  )
    
  method initial_pos i = (
    let pos = [|Random.int gridsize_; Random.int gridsize_|] in
    coord_i2f pos
  )
    
  method place_node i pos = (
    let posi = coord_f2i pos in
    assert (not (List.mem i (Graph.getinfo_ graph posi)));
    Graph.appendinfo_ graph posi i;
  )
    
  method get_nodes_within_radius data ~node ~radius = (
    let neigbor_points = Graph.nhop_and_less_neigbors_ graph ~node:(coord_f2i data.pos.(node)) ~radius:(f2i radius) in
    List.concat (List.map (fun x -> Graph.getinfo_ graph x) neigbor_points)
  )
    
  method dist a b = 
    i2f (Graph.lattice_dist_ graph ~src:(coord_f2i a) ~dest:(coord_f2i b))
      
  method zero_hop_neigbors coord1 coord2 = (coord1 = coord2)
    
  method private waypoint_move_ data ~index  =  (
      
      let target = waypoint_targets_.(index) in
      let pos = data.pos.(index) in
      if (s#dist target pos) <= 1.0 then 
	(* arrived at target *)
	begin
	  waypoint_targets_.(index) <- coord_i2f [|Random.int gridsize_; Random.int gridsize_|];
	  if index = 0 then (
	    (* should do a project_2d before drawing *)
	    Ler_graphics.circle_nodes [|waypoint_targets_.(index)|] 0.02;
	    Ler_graphics.draw_nodes [|target|];
	  );
	  target
	end
      else 
	begin
	  let direction = normalize  (target ---. pos) in
	  let increment = coord_i2f (
	    match coordf2pair (coord_round direction) with
	      | (1.0, 1.0) -> rnd_from_list [[|1; 0|]; [|0;1|]] 
	      | (1.0, -1.0) -> rnd_from_list [[|1; 0|]; [|0;-1|]] 
	      | (-1.0, 1.0) -> rnd_from_list [[|-1; 0|]; [|0;1|]] 
	      | (-1.0, -1.0) -> rnd_from_list [[|-1; 0|]; [|0;-1|]] 
	      | default -> coord_f2i (coord_round direction) 
	  )
	  in
	  if index = 0 then Ler_graphics.draw_nodes [|pos +++. increment|];
	  pos +++. increment;
	end
    )
      
      
      
  method next_position data ~index ~mob = (
      match mob with
	| RANDOMWALK -> 
	    coord_i2f (rnd_from_list (Graph.neigbors_lattice_ graph (coord_f2i data.pos.(index)) ~side:gridsize_))
	| WAYPOINT -> (
	    Printf.printf "!*!\nWaypoint model has never been tested in this
      topology. look at the node movement  and make sure it looks ok!\n!*!\n";
	    s#waypoint_move_ data ~index:index
	  )
    )

  method update_pos ~node ~oldpos ~newpos = (
    let index = node#id in
      assert (List.mem index (Graph.getinfo_ graph (coord_f2i oldpos)));
      Graph.setinfo_ graph (coord_f2i oldpos) (list_without (Graph.getinfo_ graph (coord_f2i oldpos)) index);
      Graph.appendinfo_ graph (coord_f2i newpos) index;
    )

  method route data ~src ~dst = 
    (* we can do the coord_f2i because we know that in this topology all 
       float coords are integer valued *)
    List.map 
      (fun icoord -> Coord.coord_i2f icoord)
      (Graph.route_dij_ graph 
	(Coord.coord_f2i data.pos.(src)) 
	(Coord.coord_f2i data.pos.(dst)))
      
    method scale_unit f = f /. (i2f gridsize_)

    method project_2d coord =  Array.map (fun x -> s#scale_unit x) coord 

    method get_nodes_at place = Graph.getinfo_ graph (coord_f2i (coord_round (place ***. (i2f gridsize_))))

    method sprint_info () = Printf.sprintf "\tGridsize:\t\t\t %d\n" gridsize_

  end
  



