open Misc
open Larray
open Circbuf

module type Itinerary_t = 
sig
  exception Itin_size_not_set
  type place_t = None | Place of int;;
  type t   
  val set_itin_size_ : graphsize:int -> unit  (* needed to set size of internal 'scratch' structures. could be avoided *)
  val create_ : int -> t
  val length_ : t -> int
  val addplace_ : t -> place_t -> unit
  val get_ : t -> int -> place_t    (* get an entry in the itinerary, specified by relative age *)
  val hops_to_place_ : t -> place_t -> int (* returns max_int if place not in itin *)
  val unroll_itin_ : t -> t (* itinerary must be full before this can be called *)
  val test_ : unit -> unit    
end;;

module  Itinerary : Itinerary_t = 
struct
  
  exception Itin_size_not_set
  

  type place_t = None | Place of int
  type mytype = place_t LinkedArray.linkedArray_t
  type t =  place_t CircBuf.circbuf_t (* head points to last written entry *)
		       
  let graphsize__ = ref 8
  let last_visit_of_place__ = ref (Array.create !graphsize__ max_int) (* to avoid allocating each time in unroll_itin_ *)
    
  let set_itin_size_ ~graphsize = (
    if (graphsize <= 0) then raise (Invalid_argument "Itinerary.set_itin_size_ : must be > 0");
      graphsize__ := graphsize;
      last_visit_of_place__ := Array.create graphsize max_int;
  )

  let create_ size = CircBuf.create_ size None
  let length_ itin = CircBuf.length_ itin

  let addplace_ itin place = CircBuf.push_ itin place

  let get_ itin offset = try CircBuf.get_ itin offset with Invalid_argument "Circbuf.get" -> raise (Invalid_argument "Itinerary.get_")

  let hops_to_place_ itin place = (
    (* xxx/slow this could be optimized from O(N) to O(1) by keeping an
        array of places visited *)
    let get = get_ itin in
    let rec _hops_to_place  = function
    	l when (l = length_ itin) -> max_int
      | l when ((get l) = place) -> l
      | l -> _hops_to_place (l + 1)
    in
      _hops_to_place 0
  )


				  
  let p2i__ = function 
      None -> raise (Invalid_argument "Itinerary.p2i__: Cannot convert None to int")
    | Place i -> i
	
    
  let have_wrapped__ itin =  (get_ itin (length_ itin - 1)) <> None 

  let unroll_itin_ itin = (
    
    (* last_visit_of_place__.(n) = how long ago we last visited place n in the graph  (max_int if never visited)  *)
    ArrayLabels.fill !last_visit_of_place__ ~pos:0 ~len:!graphsize__  max_int;
    
    if not (have_wrapped__ itin) then failwith "Itinerary.unroll_itin_: cannot unroll itinerary which is not full";
    
    (* forward-walk list and keep time of first visit through place *)
    (* xxx/slow since we always will iterate over whole list *)
    CircBuf.iteri_ (fun i place -> 
		      try (
			if (!last_visit_of_place__.(p2i__ place) = max_int) then !last_visit_of_place__.(p2i__ place) <- i;
		      ) with Invalid_argument "Array.get" -> 
			raise (Invalid_argument "Itinerary.unroll_itin_: itin contained place that was bigger than graphsize")
		   ) itin; 
    
    (* reverse-walk list and each time you encounter a place that's not the first encounter noted above,  *)
    (* make the shortcut. *)
    let larr = LinkedArray.create_ (CircBuf.toarray_ itin) in (* larr.(0) will be most recently visited place *)
      
    let rec revwalk t = (  
      (* recursion over t:int offset going backward in time *)

      let place = LinkedArray.get_ larr t in
      let most_recent_visit = !last_visit_of_place__.(p2i__ place) in
      let shortcut_right = if (t = LinkedArray.length_ larr - 1) then LinkedArray.None_tail else LinkedArray.Ngbr (t + 1) 
      and shortcut_left = (LinkedArray.Ngbr most_recent_visit) in

	assert (most_recent_visit <= t);

	match most_recent_visit with
	    m when (m = max_int) ->  
	      assert false

	  | m when (m = 0) ->
	      if t > m then LinkedArray.connect_ larr shortcut_left shortcut_right;
	      ()

	  | m when (m = t) -> 
	      revwalk (t - 1)

	  | m -> 
	      LinkedArray.connect_ larr shortcut_left shortcut_right;
	      revwalk (m - 1)
    )
    in
      revwalk (CircBuf.length_ itin - 1); 
      (* xxx/slow 2 allocations here. Could create a CircBuf.fromlinkedarray that skips one alloc *)
      CircBuf.fromarray_ (LinkedArray.toarray_ larr)
  )		 
			    

  let test_ () = (

    let test_unrolling size input unrolled_check = (
      let itin = create_ size in
	array_rev_iter (fun x -> addplace_ itin (Place x)) input;
	
	let unrolled = unroll_itin_ itin in
	  assert (length_ unrolled = Array.length unrolled_check);
	  for i = 0 to Array.length unrolled_check - 1 do
	    assert ((get_ unrolled i) = (Place unrolled_check.(i)))
	  done;
    ) in

    set_itin_size_ 5; (* size of graph (not itineraries!) *)

    let itin = create_ 4 in
      assert (length_ itin = 4);
      for i = 0 to 3 do
	assert ((get_ itin i) = None)
      done;
      (* 3-2-1-0 *)
      for i = 0 to 3 do
	addplace_ itin (Place i);
      done;
      for i = 0 to 3 do
	assert ((hops_to_place_ itin (Place i)) =  3 - i);
	assert ((get_ itin i) = Place (3 - i))
      done;
      (* 3-3-3-3 *)
      for i = 0 to 3 do
	addplace_ itin (Place 3);
      done;
	assert ((hops_to_place_ itin (Place 3)) = 0);
	assert ((hops_to_place_ itin (Place 2)) = max_int);
      for i = 0 to 3 do
	assert ((get_ itin i) = Place 3)
      done;


      test_unrolling 4 [|3; 2; 1; 0|] [|3; 2; 1; 0|];
      test_unrolling 2 [|3; 2; 1; 0|] [|3; 2|];

      test_unrolling 4 [|3; 3; 2; 1|] [|3; 2; 1|];
      test_unrolling 2 [|3; 3; 2; 1|] [|3|];

      test_unrolling 4 [|3; 3; 3; 2|] [|3; 2|];
      test_unrolling 4 [|3; 3; 3; 3|] [|3|];
      test_unrolling 4 [|3; 1; 2; 3|] [|3|];

      test_unrolling 4 [|3; 2; 2; 1|] [|3; 2; 1|];
      test_unrolling 4 [|3; 1; 1; 3|] [|3|];

      test_unrolling 2 [|3; 3 |] [|3|];
      test_unrolling 1 [|3|] [|3|];


      set_itin_size_ 10; (* size of graph (not itineraries!) *)

      (* incidentally, shows that current unrolling algorithm is not the best! *)
      test_unrolling 12  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 2; 9|] [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];
      test_unrolling 12  [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 2; 9; 9|] [| 0; 1; 2; 9|];


      (* cannot unroll an itin which hasn't been completely filled in *)
      try (
	test_unrolling 5 [|3; 2; 1; 0|] [|3; 2; 1; 0|];
	assert false;
      ) with 
      	  Failure "Itinerary.unroll_itin_: cannot unroll itinerary which is not full" -> ();
    Printf.printf "Itinerary.test_ : passed \n";
  )

end;;


Itinerary.test_ ()
