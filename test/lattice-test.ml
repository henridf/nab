open Misc
open Graph
open Lattice

module CoordSet = Set.Make (OrderedCoord)
open CoordSet

let coordSetofList l = (
  let c = ref CoordSet.empty in
    List.iter (fun coord -> c := CoordSet.add coord !c) l;
    !c
)

let test_ () = (
  let l = Lattice.make_lattice_ ~dim:2 ~side:2 in 
  let ngbrs = Lattice.neigbors_ l [|1; 1|] in
  let ngbrs_1hop = Lattice.nhop_neigbors_ l ~node:[|1; 1|] ~radius:1 in
  let exp_ngbrs = [[|0; 1|]; [|1; 0|]] 
  in
    assert (list_same  ngbrs exp_ngbrs);
    assert (list_same  ngbrs_1hop exp_ngbrs);
    
  let l = Lattice.make_lattice_ ~dim:2 ~side:5 in 
  let ngbrs1h = Lattice.nhop_neigbors_ l ~node:[|1; 1|] ~radius:1 in
  let ngbrs2h = Lattice.nhop_neigbors_ l ~node:[|2; 2|] ~radius:2 in
  let ngbrs4h = Lattice.nhop_neigbors_ l ~node:[|2; 2|] ~radius:4 in
  let ngbrs5h = Lattice.nhop_neigbors_ l ~node:[|2; 2|] ~radius:5 in
  let exp_ngbrs1h = [[|0; 1|]; [|1; 0|]; [|2; 1|]; [|1; 2|]]  in
  let exp_ngbrs2h = [[|2; 0|]; [|0; 2|]; [|4; 2|]; [|2; 4|]; [|3; 3|]; [|3; 1|]; [|1; 3|]; [|1; 1|]] in
  let exp_ngbrs4h = [[|0; 0|]; [|4; 0|]; [|4; 4|]; [|0; 4|]] in
  let exp_ngbrs5h = [] 
  in
    assert (list_same  ngbrs1h exp_ngbrs1h);
    assert (list_same  ngbrs2h exp_ngbrs2h);
    assert (list_same  ngbrs4h exp_ngbrs4h);
    assert (list_same  ngbrs5h exp_ngbrs5h);

    (* routing *)
    assert (Lattice.route_dij_ l ~src:[|2; 2|] ~dest:[|4; 2|] =
	      [[|3; 2|]; [|4; 2|]]);
    assert (
      (Lattice.route_dij_ l ~src:[|0; 0|] ~dest:[|4; 4|] = [[|0; 4|]; [|4; 4|]])
      || 
      (Lattice.route_dij_ l ~src:[|0; 0|] ~dest:[|4; 4|] = [[|4; 0|]; [|4; 4|]])
    );
    assert (List.length (Lattice.route_dij_ l ~src:[|1; 1|] ~dest:[|4; 4|] ) = 4);

    let l = Lattice.make_lattice_ ~dim:3 ~side:3 in 
    let ngbrs = Lattice.neigbors_ l [|2; 2; 2|] 
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
      Printf.printf "Lattice.test_ : passed \n";
      

)
		 

let _ = test_ ()


