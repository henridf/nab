open Misc 
open Hashtbl
open Graph
open Printf
open Coord

module OrderedCoord = 
struct 
  type t = int array
  let compare coord1 coord2 = 
    if (normdot coord1) < (normdot coord2) then -1 
    else if (normdot coord1) > (normdot coord2) then 1 
    else 0
end 

module CoordSet = Set.Make (OrderedCoord)
open CoordSet

let coordSetofList l = (
  let c = ref CoordSet.empty in
  List.iter (fun coord -> c := CoordSet.add coord !c) l;
  !c
)

let test_ () = (

  (* 1. GENERAL GRAPHS *)
  let g = Graph.make_ 0.0 10 Graph.Undirected in 
  (* 1.1 ADDING NODES AND EDGES *)
  assert (Graph.contains_ g 0.0 = false);
  assert (Graph.containsi_ g 0 = false);
  Graph.add_node_ g 0.0;
  assert (Graph.contains_ g 0.0 = true);
  assert (Graph.containsi_ g 0 = true);
  assert (Graph.neigbors_ g 0.0 = []);
  assert (Graph.neigborsi_ g 0 = []);
  
  Graph.add_node_ g 1.0;
  Graph.add_edgei_ g 0 1 0.0;
  assert (Graph.neigbors_ g 0.0 = [1.0]);
  assert (Graph.neigbors_ g 1.0 = [0.0]);
  
  Graph.add_node_ g 2.0;
  Graph.add_node_ g 3.0;
  Graph.add_edge_ g 2.0 3.0 0.0;
  assert (Graph.neigborsi_ g 2 = [3]);
  assert (Graph.neigborsi_ g 3 = [2]);
  
  Graph.add_edge_ g 0.0 2.0 0.0;
  Graph.add_edge_ g 0.0 3.0 0.0;
  Graph.add_edge_ g 2.0 1.0 0.0;
  Graph.add_edge_ g 3.0 1.0 0.0;
  

  (* fully connected
     0 -> 1 2 3 
     1 -> 0 2 3 
     2 -> 0 1 3 
     3 -> 0 1 2 
  *)

  (* 1.2 NEIGBORS  *)

  let ngbrs = Graph.neigborsi_ g 1 
  and ngbrs_1hop = Graph.nhop_neigborsi_ g ~index:1 ~radius:1 
  and ngbrs_0hop = Graph.nhop_neigborsi_ g ~index:1 ~radius:0 
  and ngbrs_2hop = Graph.nhop_neigborsi_ g ~index:1 ~radius:2 
  and ngbrs_2hop_and_less = Graph.nhop_and_less_neigborsi_ g ~index:1 ~radius:2 in
  assert (List.length ngbrs = 3);
  assert (list_same ngbrs [0; 2; 3]);
  assert (list_same ngbrs ngbrs_1hop);
  assert (ngbrs_2hop = []);
  assert (ngbrs_0hop = [1]);
  assert (list_same ngbrs_2hop_and_less [0; 1; 2; 3]);

  (* 1.3 ROUTING *)

  assert (Graph.routei_dij_ g ~src:0 ~dest:1 = [0]);
  assert (Graph.disti_ g ~src:0 ~dest:1 = 1);
  assert (Graph.routei_dij_ g ~src:0 ~dest:0 = []);
  assert (Graph.disti_ g ~src:0 ~dest:0 = 0);
  (* connect a string 4-5-6 off of 3 *)
  Graph.add_node_ g 4.0; Graph.add_edge_ g 3.0 4.0 0.0;
  Graph.add_node_ g 5.0; Graph.add_edge_ g 4.0 5.0 0.0;
  Graph.add_node_ g 6.0; Graph.add_edge_ g 5.0 6.0 0.0;
  assert (Graph.routei_dij_ g ~src:0 ~dest:6 = [ 0; 3; 4; 5]);
  Graph.add_edge_ g 4.0 6.0 0.0;
  assert (Graph.routei_dij_ g ~src:0 ~dest:6 = [ 0; 3; 4]);
  assert (Graph.route_dij_ g ~src:0.0 ~dest:6.0 = [0.0; 3.0; 4.0]);
  assert (Graph.dist_ g ~src:0.0 ~dest:6.0 = 3);

  (* add unreachable node 7 *)
  Graph.add_node_ g 7.0;
  begin
    try (
      ignore (Graph.routei_dij_ g ~src:0 ~dest:7);
      assert (false);
    ) with 
	Failure "No_route" -> ();
  end;
  let g = Graph.make_ 0.0 10 Graph.Directed in 
  Graph.add_node_ g 0.0;
  Graph.add_node_ g 1.0;
  Graph.add_node_ g 2.0;
  Graph.add_node_ g 3.0;
  
  Graph.add_edgei_ g 0 1 0.0;
  assert (Graph.neigborsi_ g 0 = [1]);
  assert (Graph.neigborsi_ g 1 = []);

  (* 1.4 INFOS *)

  assert ((Graph.getinfo_ g 0.0) = []);
  assert ((Graph.getinfoi_ g 0) = []);
  Graph.setinfo_ g 0.0 [0;0];
  Graph.setinfoi_ g 1 [1;1];
  assert ((Graph.getinfo_ g 0.0) = [0;0]);
  assert ((Graph.getinfoi_ g 0) = [0;0]);
  assert ((Graph.getinfo_ g 1.0) = [1;1]);
  assert ((Graph.getinfoi_ g 1) = [1;1]);
  assert ((Graph.getinfosi_ g [0;1]) = [0;0;1;1]);
  Graph.appendinfo_ g 2.0 10;
  Graph.appendinfoi_ g 2 11;
  assert (list_same (Graph.getinfo_ g 2.0) [10; 11]);
  assert (list_same (Graph.getinfoi_ g 2) [10; 11]);
  

  (* 2.0 WRAPPING LATTICES *)
  
  (* 2.1 DIMENSION *)
  assert ((Graph.lattice_dim_ ~maxsize:1 ~side:1) = 1);
  assert ((Graph.lattice_dim_ ~maxsize:10 ~side:10) = 1);
  assert ((Graph.lattice_dim_ ~maxsize:100 ~side:10) = 2);
  assert ((Graph.lattice_dim_ ~maxsize:1000 ~side:10) = 3);

  (* 2.2 NEIGBORS *)
  let l = Graph.make_wrap_lattice_ ~dim:2 ~side:2 in 
  let ngbrs = Graph.neigbors_ l [|1; 1|] in
  let ngbrs_1hop = Graph.nhop_neigbors_ l ~node:[|1; 1|] ~radius:1 in
  let exp_ngbrs = [[|0; 1|]; [|1; 0|]] 
  in
  assert (list_same ngbrs exp_ngbrs);
  assert (list_same ngbrs_1hop exp_ngbrs);
  
  let l = Graph.make_wrap_lattice_ ~dim:2 ~side:5 in 
  let ngbrs0h = Graph.nhop_neigbors_ l ~node:[|2; 2|] ~radius:0 in
  let ngbrs0h_and_less = Graph.nhop_and_less_neigbors_ l ~node:[|2; 2|] ~radius:0 in
  let ngbrs1h = Graph.nhop_neigbors_ l ~node:[|2; 2|] ~radius:1 in
  let ngbrs1h_and_less = Graph.nhop_and_less_neigbors_ l ~node:[|2; 2|] ~radius:1 in
  let ngbrs2h = Graph.nhop_neigbors_ l ~node:[|2; 2|] ~radius:2 in
  let ngbrs2h_and_less = Graph.nhop_and_less_neigbors_ l ~node:[|2; 2|] ~radius:2 in
  let ngbrs3h = Graph.nhop_neigbors_ l ~node:[|2; 2|] ~radius:3 in
  let ngbrs3h_and_less = Graph.nhop_and_less_neigbors_ l ~node:[|2; 2|] ~radius:3 in
  let ngbrs4h = Graph.nhop_neigbors_ l ~node:[|2; 2|] ~radius:4 in
  let ngbrs4h_and_less = Graph.nhop_and_less_neigbors_ l ~node:[|2; 2|] ~radius:4 in
  let ngbrs5h = Graph.nhop_neigbors_ l ~node:[|2; 2|] ~radius:5 in
  let ngbrs5h_and_less = Graph.nhop_and_less_neigbors_ l ~node:[|2; 2|] ~radius:5 in
  let exp_ngbrs0h = [[|2; 2|]] in
  let exp_ngbrs1h = [[|1; 2|]; [|2; 1|]; [|3; 2|]; [|2; 3|]]  in
  let exp_ngbrs2h = [[|2; 0|]; [|0; 2|]; [|4; 2|]; [|2; 4|]; [|3; 3|]; [|3; 1|]; [|1; 3|]; [|1; 1|]] in
  let exp_ngbrs3h = [[|4; 1|]; [|1; 4|]]  in
  let exp_ngbrs4h = [[|0; 0|]; [|4; 0|]; [|4; 4|]; [|0; 4|]] in
  let exp_ngbrs5h = [] 
  in
  assert (list_same ngbrs0h exp_ngbrs0h);
  assert (list_same ngbrs1h exp_ngbrs1h);
  assert (list_same ngbrs2h exp_ngbrs2h);
  assert (list_same ngbrs4h exp_ngbrs4h);
  assert (list_same ngbrs5h exp_ngbrs5h);
  assert ((List.length ngbrs5h_and_less) = 25);
  assert (list_same ngbrs1h_and_less (ngbrs0h @ ngbrs1h));
  assert (list_same ngbrs2h_and_less (ngbrs0h @ ngbrs1h @ ngbrs2h));
  assert (list_same ngbrs3h_and_less (ngbrs0h @ ngbrs1h @ ngbrs2h @ ngbrs3h));
  assert (list_same ngbrs4h_and_less (ngbrs0h @ ngbrs1h @ ngbrs2h @ ngbrs3h @ ngbrs4h));
  assert (list_same ngbrs5h_and_less (ngbrs0h @ ngbrs1h @ ngbrs2h @ ngbrs3h @ ngbrs4h @ ngbrs5h));

  let g = Graph.make_wrap_lattice_ ~dim:3 ~side:5 in
  Graph.itern_ (fun n -> 
    assert (
      list_same (Graph.neigbors_ g n) (Graph.neigbors_lattice_wrap_ g n ~side:5 )
    ) 
  ) g;
    
  let g = Graph.make_wrap_lattice_ ~dim:3 ~side:5 in
  Graph.iteri_ (fun i -> 
    assert (
      list_same (Graph.neigborsi_ g i) (Graph.neigborsi_lattice_wrap_ g ~index:i ~side:5 )
    ) 
  ) g;

  (* 2.3 ROUTING *)

  assert (Graph.route_dij_ l ~src:[|2; 2|] ~dest:[|4; 2|] =
    [[|2; 2|]; [|3; 2|]]);
  assert (
    (Graph.route_dij_ l ~src:[|0; 0|] ~dest:[|4; 4|] = [[|0; 0|]; [|0; 4|]])
    || 
    (Graph.route_dij_ l ~src:[|0; 0|] ~dest:[|4; 4|] = [[|0; 0|]; [|4; 0|]])
  );
  assert (List.length (Graph.route_dij_ l ~src:[|1; 1|] ~dest:[|4; 4|] ) = 4);

  let l = Graph.make_wrap_lattice_ ~dim:3 ~side:8 in
  assert ((Graph.dist_ l [|0;0;0|] [|2;2;2|]) = 6);
  assert ((Graph.lattice_dist_ l [|0;0;0|] [|2;2;2|]) = 6);


  let l = Graph.make_wrap_lattice_ ~dim:3 ~side:3 in 
  let ngbrs = Graph.neigbors_ l [|2; 2; 2|] 
  and exp_ngbrs =  [
    [|0; 2; 2|]; 
    [|1; 2; 2|]; 
    [|2; 0; 2|];
    [|2; 1; 2|];
    [|2; 2; 0|];
    [|2; 2; 1|];
  ]
  in
  assert (list_same  ngbrs exp_ngbrs);

  (* 3.0 NONWRAPPING LATTICES *)

  (* 3.2 NEIGBORS *)
  let g = Graph.make_lattice_ ~dim:3 ~side:5 in
  Graph.itern_ (fun n -> 
    assert (
      list_same (Graph.neigbors_ g n) (Graph.neigbors_lattice_ g n ~side:5 )
    ) 
  ) g;
    
  let g = Graph.make_lattice_ ~dim:3 ~side:5 in
  Graph.iteri_ (fun i -> 
    assert (
      list_same (Graph.neigborsi_ g i) (Graph.neigborsi_lattice_ g ~index:i ~side:5 )
    ) 
  ) g;




  Printf.printf "Graph.test_ : passed \n";

)   
  
let _ = test_  ()		 


