open Misc
open Coord
open Graph
open Common

module Lat2D : Top.Topology_t = 
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
      
    let dist data ~src ~dest = 
      i2f (Graph.lattice_dist_ !g ~src:(coord_f2i data.pos.(src)) ~dest:(coord_f2i data.pos.(dest)))
	
    let zero_hop_neigbors coord1 coord2 = (coord1 = coord2)
      
    let next_position data ~index = 
      coord_i2f (rnd_from_list (Graph.neigbors_lattice_ !g (coord_f2i data.pos.(index)) ~side:!gridsize_))

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
  



