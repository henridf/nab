open Misc
open Larray
open Circbuf
open Itin
open Printf

let test_ () = (
  
  let graphsize = ref 5 in
  let make_itin size input = (
    let itin = Itinerary.make_ ~itinsize:size ~graphsize:!graphsize in 
      array_rev_iter (fun x -> Itinerary.addplace_ itin  x) input; itin;
  ) in
    
  let test_unrolling size input unrolled_check = (
    let itin = make_itin size input in
    let unrolled = Itinerary.unroll_ itin in
      assert (Itinerary.maxlength_ unrolled = Array.length unrolled_check);
      for i = 0 to Array.length unrolled_check - 1 do
	assert ((Itinerary.get_ unrolled i) = unrolled_check.(i));
	assert ((Itinerary.hops_to_place_ unrolled unrolled_check.(i)) == i);
      done;
    ) in

    (* semantic equality *)
    assert (Itinerary.equal_ 
	      (make_itin 4 [| 1; 2; 3; 4|]) 
	      (make_itin 6 [| 1; 2; 3; 4|])
	   );
    assert (not 
	      (Itinerary.equal_ 
		 (make_itin 4 [| 1; 2; 3; 4|]) 
		 (make_itin 4 [| 1; 2; 3|])
	      )
	   );

    assert (Itinerary.equal_
	      (make_itin 1 [||])
	      (make_itin 2 [||])
	   );

    assert (Itinerary.equal_
	      (make_itin 0 [||])
	      (make_itin 0 [||])
	   );

    (* sub itineraries *)
    assert (Itinerary.equal_ 
	      (Itinerary.sub_
		 (make_itin 4 [| 4; 3; 2; 1|])
		 3)
	      (make_itin 2 [| 4; 3; 2; 1|])
	   );
    assert (Itinerary.equal_ 
	      (Itinerary.sub_
		 (make_itin 4 [| 4; 3; 2; 1|])
		 1)
	      (make_itin 4 [| 4; 3; 2; 1|])
	   );	      

    (* has_place_ *)
    assert (Itinerary.has_place_
	      (make_itin 2 [| 4; 3; 2; 1|])
	      3
	   );

    assert (not
	      (Itinerary.has_place_
		 (make_itin 2 [| 4; 3; 2; 1|])
		 2)
	   );
    
    let itin = Itinerary.make_ ~itinsize:4 ~graphsize:!graphsize in
      assert (Itinerary.maxlength_ itin = 4);
      for i = 0 to 3 do
	try (
	  ignore (Itinerary.get_ itin i); 
	  assert false;
	) with 
	    Invalid_argument "Itinerary.get_ : No value at this index" -> ();
      done;

      
      (* 3-2-1-0 *)
      for i = 0 to 2 do
	Itinerary.addplace_ itin i;
      done;
      try (
	assert ((Itinerary.hops_to_place_ itin  3) <> max_int);
	assert false;
      ) with 
	  Failure "Itinerary.hops_to_place_ : place has never been visited" -> ();
      Itinerary.addplace_ itin 3;

      for i = 0 to 3 do
	assert ((Itinerary.hops_to_place_ itin  i) =  3 - i);
	assert (Itinerary.get_ itin (Itinerary.hops_to_place_ itin  i) =  i);
	assert ((Itinerary.get_ itin i) = (3 - i))
      done;
      (* 3-3-3-3 *)
      for i = 0 to 3 do
	Itinerary.addplace_ itin 3;
      done;
      assert ((Itinerary.hops_to_place_ itin  3) = 0);

      try (
	assert ((Itinerary.hops_to_place_ itin  2) <> 0);
	assert false;
      ) with 
	  Failure "Itinerary.hops_to_place_ : place visited, but out of itinerary" -> ();

      for i = 0 to 3 do
	assert ((Itinerary.get_ itin i) =  3)
      done;

      
      test_unrolling 1 [|1|] [|1|];
      test_unrolling 4 [|1|] [|1|];


      test_unrolling 1 [||] [||];
(*
      test_unrolling 0 [||] [||];
*)
      test_unrolling 4 [|3; 2; 1; 0|] [|3; 2; 1; 0|];
      test_unrolling 6 [|3; 2; 1; 0|] [|3; 2; 1; 0|];

      test_unrolling 2 [|3; 2; 1; 0|] [|3; 2|];

      test_unrolling 4 [|3; 3; 2; 1|] [|3; 2; 1|];
      test_unrolling 6 [|3; 3; 2; 1|] [|3; 2; 1|];

      test_unrolling 2 [|3; 3; 2; 1|] [|3|];

      test_unrolling 4 [|3; 3; 3; 2|] [|3; 2|];
      test_unrolling 6 [|3; 3; 3; 2|] [|3; 2|];

      test_unrolling 4 [|3; 3; 3; 3|] [|3|];
      test_unrolling 4 [|3; 1; 2; 3|] [|3|];

      test_unrolling 4 [|3; 2; 2; 1|] [|3; 2; 1|];
      test_unrolling 6 [|3; 2; 2; 1|] [|3; 2; 1|];

      test_unrolling 4 [|3; 1; 1; 3|] [|3|];
      test_unrolling 6 [|3; 1; 1; 3|] [|3|];

      test_unrolling 2 [|3; 3 |] [|3|];
      test_unrolling 6 [|3; 3 |] [|3|];

      test_unrolling 1 [|3|] [|3|];


      graphsize := 10;

      (* incidentally, shows that current unrolling algorithm is not the best! *)
      test_unrolling 12  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 2; 9|] [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];
      test_unrolling 20  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 2; 9|] [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];

      test_unrolling 12  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 2; 9; 9|] [| 0; 1; 2; 9|];
      test_unrolling 20  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 2; 9; 9|] [| 0; 1; 2; 9|];


      Printf.printf "Itinerary.test_ : passed \n";


    (* Itinerary.shorten_ *)
    (* 1-2-7  4-5-6-7 *)
    graphsize := 11;
    let it1 = make_itin 4 [| 4; 5; 6; 7|] in
    let it2 = make_itin 3 [| 1; 2; 7|] in
    let shortened1 = Itinerary.shorten_ ~aux:it1 ~main:it2 
    and shortened2 = Itinerary.shorten_ ~aux:it2 ~main:it1 in
      assert (shortened1 = make_itin 3 [| 1; 2; 7|]);
      assert (shortened2 = make_itin 3 [| 1; 2; 7|]);

   let it1 = make_itin 6 [| 4; 5; 6; 7|] in
   let it2 = make_itin 7 [| 1; 2; 7|] in
   let shortened1 = Itinerary.shorten_ ~aux:it1 ~main:it2 in
   let shortened2 = Itinerary.shorten_ ~aux:it2 ~main:it1 in
     assert (Itinerary.equal_ shortened1 ( make_itin 3 [| 1; 2; 7|]));
     assert (Itinerary.equal_ shortened2 ( make_itin 3 [| 1; 2; 7|]));


    let it1 = make_itin 9 [|1; 2; 3; 5; 6; 7; 8; 9; 10|] in
    let it2 = make_itin 6 [|3; 4; 5; 6; 9; 10|] in
    let shortened1 = Itinerary.shorten_ ~aux:it1 ~main:it2 
    and shortened2 = Itinerary.shorten_ ~aux:it2 ~main:it1 in
      assert (shortened1 = make_itin 6 [|3; 4; 5; 6; 9; 10|]);
      assert (shortened2 = make_itin 6 [|3; 4; 5; 6; 9; 10|]);
      
      let it1 = make_itin 12 [|1; 2; 3; 5; 6; 7; 8; 9; 10|] in
    let it2 = make_itin 6 [|3; 4; 5; 6; 9; 10|] in
    let shortened1 = Itinerary.shorten_ ~aux:it1 ~main:it2 
    and shortened2 = Itinerary.shorten_ ~aux:it2 ~main:it1 in
     assert (Itinerary.equal_ shortened1 ( make_itin 6 [|3; 4; 5; 6; 9; 10|]));
     assert (Itinerary.equal_ shortened2 ( make_itin 6 [|3; 4; 5; 6; 9; 10|]));

    let it1 = make_itin 9 [|1; 2; 3; 5; 6; 7; 8; 9; 10|] 
    and it2 = make_itin 9 [|1; 2; 3; 5; 6; 7; 8; 9; 10|] in
    let shortened1 = Itinerary.shorten_ ~aux:it1 ~main:it2 
    and shortened2 = Itinerary.shorten_ ~aux:it2 ~main:it1 in
      assert (shortened1 = it1);
      assert (shortened2 = it1);

      graphsize := 25;
      let it1 = make_itin 14 [|17; 18; 14; 15; 20; 19; 23; 3; 24; 4; 5; 10|] 
      and it2 = make_itin 11 [|2; 22; 23; 18; 17; 12; 13; 9; 14; 15; 10 |] in
      let shortened1 = Itinerary.shorten_ ~aux:it1 ~main:it2 
      and shortened2 = Itinerary.shorten_ ~aux:it2 ~main:it1 in
	(* shorten_ is not commutative! *)
	assert (Itinerary.equal_ shortened1 (make_itin 5 [|17; 18; 14; 15; 10|]));
	assert (Itinerary.equal_ shortened2 (make_itin 8 [|2; 22; 23; 3; 24; 4; 5; 10|]));

	
    (* Itinerary.splice_ *)
    (* splice 3-2-1-0 with 3-2-1-0 at any place -> same itin*)
    let itin = make_itin 4 [|3; 2; 1; 0|]
    and itin2 = make_itin 4 [|3; 2; 1; 0|] in 
    let itin_l = make_itin 5 [|3; 2; 1; 0|]
    and itin2_l = make_itin 6 [|3; 2; 1; 0|] in 
      begin
	for i = 0 to 3 do
	  assert ((Itinerary.splice_ itin itin2 i) = itin);
	  assert (Itinerary.maxlength_ (Itinerary.splice_ itin itin2 i) =  4);
	  assert ((Itinerary.splice_ itin_l itin2_l i) = itin);
	  assert (Itinerary.maxlength_ (Itinerary.splice_ itin_l itin2_l i) =  4);
	done;
      end;
      
    (* splice 3-2-3-2 with 3-2-1-0 at 2 or 3 -> same itin*)
    let itin = make_itin 4 [|3; 2; 3; 2|]
    and itin2 = make_itin 4 [|3; 2; 1; 0|] in 
    let itin_l = make_itin 5 [|3; 2; 3; 2|]
    and itin2_l = make_itin 5 [|3; 2; 1; 0|] in 
      begin
	assert ((Itinerary.splice_ itin itin2  2) = itin2);
	assert (Itinerary.maxlength_ (Itinerary.splice_ itin itin2 2) =  4);
	assert (Itinerary.maxlength_ (Itinerary.splice_ itin itin2 3) =  4);
	assert ((Itinerary.splice_ itin itin2  3) = itin2);
	assert ((Itinerary.splice_ itin_l itin2_l  2) = itin2);
	assert ((Itinerary.splice_ itin_l itin2_l  3) = itin2);
	assert (Itinerary.maxlength_ (Itinerary.splice_ itin_l itin2_l 2) =  4);
	assert (Itinerary.maxlength_ (Itinerary.splice_ itin_l itin2_l 3) =  4);
      end;

    (* splice 0-0-0-0 with 3-2-1-0 at 0 -> 0*)
    let itin = make_itin 4 [|0; 0; 0; 0;|]
    and itin2 = make_itin 4 [|3; 2; 1; 0|] in 
    let itin_l = make_itin 5 [|0; 0; 0; 0;|]
    and itin2_l = make_itin 5 [|3; 2; 1; 0|] in 
      begin
	let newitin = Itinerary.splice_ itin itin2  0 
	and newitin_l = Itinerary.splice_ itin_l itin2_l  0 
	in
	  assert ((Itinerary.maxlength_ newitin) = 1);
	  assert ((Itinerary.get_ newitin 0) =  0);
	  assert ((Itinerary.hops_to_place_ newitin  0) = 0) ;
	  assert ((Itinerary.maxlength_ newitin_l) = 1);
	  assert ((Itinerary.get_ newitin_l 0) =  0);
	  assert ((Itinerary.hops_to_place_ newitin_l  0) = 0) ;
      end;
      
    (* splice 1-3-0 with 3-2-1-0 at  0 -> 1-3-0*)
    let itin = make_itin 4 [|1; 3; 0|]
    and itin2 = make_itin 4 [|3; 2; 1; 0|] in 
      begin
	let newitin = Itinerary.splice_ itin itin2  0 in
	  assert ((Itinerary.maxlength_ newitin) = 3);
	  assert ((Itinerary.get_ newitin 0) = 1);
	  assert ((Itinerary.hops_to_place_ newitin  1) = 0) ;
	  assert ((Itinerary.get_ newitin 1) =  3);
	  assert ((Itinerary.hops_to_place_ newitin 3) = 1) ;
	  assert ((Itinerary.get_ newitin 2) =  0);
	  assert ((Itinerary.hops_to_place_ newitin 0) = 2) ;
      end;

      (*   Itinerary.reverse_ *)
      assert (Itinerary.equal_ 
		(Itinerary.reverse_
		   (make_itin 4 [| 4; 3; 2; 1|])
		)
		(make_itin 4 [| 1; 2; 3; 4|])
	     );	      

      assert (Itinerary.equal_ 
		(Itinerary.reverse_
		   (make_itin 4 [| 1|])
		)
		(make_itin 4 [| 1|])
	     );	      

      assert (Itinerary.equal_ 
		(Itinerary.reverse_
		   (make_itin 4 [||])
		)
		(make_itin 4 [||])
	     );	      

      assert (Itinerary.equal_ 
		(Itinerary.reverse_ 
		   (
		     (Itinerary.reverse_
			(make_itin 6 [|1; 2; 3; 4; 5; 2; 3; 4|])
		     )
		   )
		)
		(make_itin 6 [|1; 2; 3; 4; 5; 2; 3; 4|])
	     );

      (* Itinerary.iteri_ *)
      let a = Array.make 5 0 in
	Itinerary.iteri_ (fun i x -> a.(i) <- x) (make_itin 6 [|1; 2; 3; 4; 7|]);
	assert (a = [|1; 2; 3; 4; 7|]);

      (* Itinerary.toarray_ *)
      assert (Itinerary.toarray_ (make_itin 6 [|1; 2; 3; 4; 7|]) = [|1; 2; 3; 4; 7|]);
      assert (Itinerary.toarray_ (make_itin 6 [||]) = [||]);
)
		 
		 
		 
let _ = test_ ()
	    
