(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Continuous topology with reflective boundaries                                      *)

open Coord
open Misc
open Common


(* This class duplicates code from  contTaurusTop.ml, keep in mind when 
   changing things *)

class contRefTop : Topology.topology = 
object(s)

  val mutable grid_of_nodes_ =  (Array.make_matrix 1 1 [])
    
  val mutable gridsize_ =  1.0 (* in practice an int, but stored as float to avoid many i2f's*)
  val mutable center_ =  [|0.5; 0.5|]
  val mutable waypoint_targets_ = (Array.make 1 [|0.0; 0.0|])

  initializer (
    let g = round (sqrt (i2f (Param.get Params.nodes))) in
    gridsize_ <- g;
    center_ <- [|g /. 2.0; g /. 2.0|];
    grid_of_nodes_ <- (Array.make_matrix (f2i g) (f2i g) []);
    waypoint_targets_ <- (
      let w = ref (Array.make (Param.get Params.nodes) ([|0.0; 0.0|]:coordf_t)) in
      Array.iteri (
	fun i nothing  -> 
	  !w.(i) <- [|Random.float g; Random.float g|]
      ) !w;
      !w
    )
  )

  method private pos_in_grid_ pos = coord_f2i (coord_floor pos)

  (* return an initial pos for node i *)
  method initial_pos i = (
    let pos = [|Random.float gridsize_; Random.float gridsize_|] in
    let (x, y) = ((s#pos_in_grid_ pos).(0), (s#pos_in_grid_ pos).(1)) in
    pos
  )

  (* set position internally *)
  method place_node i pos = (
    let (x, y) = ((s#pos_in_grid_ pos).(0), (s#pos_in_grid_ pos).(1)) in
    assert (not (List.mem i grid_of_nodes_.(x).(y)));
    grid_of_nodes_.(x).(y) <- i::grid_of_nodes_.(x).(y);
  )

  method private reflect_ pos = (
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

  method dist a b = sqrt (Coord.dist_sq a b)
  method zero_hop_neigbors pos1 pos2 = (coord_round pos1) = (coord_round pos2)



  method private waypoint_move_ data ~index  =  (
    
    let target = waypoint_targets_.(index) in
    let pos = data.pos.(index) in
    if (s#dist target pos) <= 1.0 then 
      (* arrived at target *)
      begin
	waypoint_targets_.(index) <- [|Random.float gridsize_; Random.float gridsize_|];
	target
      end
    else 
      begin
	let direction = normalize (target ---. pos) in
	pos +++. direction;
      end
  )
    
  method next_position data ~index ~mob = (
    match mob with
      | RANDOMWALK -> 
	  s#reflect_ (
	    data.pos.(index) +++. ([|Random.float 2.0; Random.float 2.0|] ---. [|1.0; 1.0|])
	  )
      | WAYPOINT -> s#waypoint_move_ data ~index:index
  )

  method update_pos ~index ~oldpos ~newpos = (
    let (oldx, oldy) = ((s#pos_in_grid_ oldpos).(0), (s#pos_in_grid_ oldpos).(1)) in
    let (newx, newy) = ((s#pos_in_grid_ newpos).(0), (s#pos_in_grid_ newpos).(1)) in
    assert (List.mem index (grid_of_nodes_.(oldx).(oldy)));
    grid_of_nodes_.(oldx).(oldy) <- list_without grid_of_nodes_.(oldx).(oldy) index;      
    grid_of_nodes_.(newx).(newy) <- index::grid_of_nodes_.(newx).(newy);      
  )
    
  method get_nodes_within_radius data ~node ~radius = (
    
    let radius_sq = radius ** 2.0 in
    let center = data.pos.(node) in
    let l = ref [] in
    Array.iteri (fun node pos -> if s#dist center pos <= radius then l := node::!l) data.pos;
    !l
  ) 

  (* for now, route in continuous domain is direct path *)
  method route data ~src ~dst = [data.pos.(src)]

  method scale_unit f = f /. gridsize_

  method project_2d coord =  Array.map (fun x -> s#scale_unit x) coord 

  method get_nodes_at pos = 
    let scaleup = pos ***. gridsize_ in
    let (x,y) = ((s#pos_in_grid_ scaleup).(0), (s#pos_in_grid_ scaleup).(1)) in
    grid_of_nodes_.(x).(y)

  method sprint_info () = Printf.sprintf "\tGridsize:\t\t\t %f\n" gridsize_

end


