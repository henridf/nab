open Larray
open Circbuf

module type Itinerary_t = 
sig
  exception Itin_size_not_set
  type place_t = None | Place of int;;
  type itinerary_t   
  val set_itin_size_ : graphsize:int -> unit  (* needed to set size of internal 'scratch' structures. could be avoided *)
  val create_ : int -> itinerary_t
  val length_ : itinerary_t -> int
  val addplace_ : itinerary_t -> int -> unit
  val get_ : itinerary_t -> int -> place_t    (* get an entry in the itinerary, specified by relative age *)
  val hops_to_place_ : itinerary_t -> place_t -> int (* returns max_int if place not in itin *)
  val unroll_itin_ : itinerary_t -> itinerary_t
  val test_ : unit -> unit    
end;;

module  Itinerary : Itinerary_t = 
struct
  
  exception Itin_size_not_set
  
  type place_t = None | Place of int
  type itinerary_t = place_t circbuf (* head points to last written entry *)
		       
  let _graphsize = ref 8
  let _scratch_unroll_itin = ref (Array.create 0 None) (* allocate once and for all (used in unroll_itin_) *)
    
  let set_itin_size_ ~graphsize = (
    if (graphsize <= 0) then failwith "Itinerary.set_itin_size_ : must be > 0";
      _graphsize := graphsize;
      _scratch_unroll_itin := Array.create graphsize max_int;
      (* initialize scratch arrays *)
  )

  let create_ size = Circbuf.create size None
  let length_ itin = Circbuf.length itin

  let addplace_ itin place = Circbuf.push_ itin place

  let get_ itin offset = Circbuf.get_ itin offset

  let hops_to_place_ itin place = (
    (* xxx/slow this could be optimized from O(N) to O(1) by keeping an
        array of places visited *)
    let get = get_ itin in
    let rec _hops_to_place  = function
	l when ((get l) = place) -> l
      |	l when (l = length_ itin) -> max_int
      | l -> _hops_to_place (l + 1)
    in
      _hops_to_place 0
  )


				  
  let p2i = function 
      None -> failwith "Cannot convert None to int"
    | Place i -> i
	
    
  let have_wrapped itin =  (get_ itin (length_ itin - 1)) <> None 

  let unroll_itin_ itin = (
    
    (* _scratch_unroll_itin.(n) = how long ago we last visited place n in the graph  (max_int if never visited)  *)
    ArrayLabels.fill !_scratch_unroll_itin ~pos:0 ~len:!_graphsize  max_int;
    
    (* forward-walk list and keep time of first visit through place *)
    (* xxx/slow since we always will iterate over whole list *)
    if not (have_wrapped itin) then failwith "Itinerary.unroll_itin_: cannot unroll itinerary which is not full"
    Circbuf.iteri_ (fun i place -> 
		      if (!_scratch_unroll_itin.(p2i place) = max_int) then !_scratch_unroll_itin.(p2i place) <- i;
		   ) itin; 

    (* reverse-walk list and each time you encounter 
       a place that's not the first encounter noted above, make the shortcut. *)
    let places_head_at_start = array.create (Array.length itin.places) None in
      Array.blit itin.places itin.head places_head_at_start 0 (Array.length itin.places - itin.head;
      Array.blit itin.places 0 places_head_at_start (Array.length itin.places - itin.head) itin.head;
    let larr = LinkedArray.create_ itin.places in
    let rec revwalk  = function 
      None -> ()
    | i -> 
	let place = LinkedArray.get_ larr i  in
	let firstenc = !_scratch_unroll_itin.(p2i place) in
	  assert (firstenc <= i);
	  if (firstenc < i) && (firstenc <> -1) then (
	    LinkedArray.connect_ larr firstenc i; (* make shortcut *)
	    revwalk firstenc;
	  ) 
	  else revwalk (i - 1)
     in
      revwalk (Array.length itin.places - 1); (* can't do an Array.length on LinkedArray since abstract*)
      {places = LinkedArray.toarray_ larr; head=0}
  )
			 
  let test_ () = (
    set_itin_size_ 4; (* itineraries on a graph of size 4 *)
    let itin = create_ 4 in
      assert (length_ itin = 4);
      for i = 0 to 3 do
	assert ((get_ itin i) = None)
      done;
      addplace_ itin 0;
      addplace_ itin 1;
      addplace_ itin 2;
      addplace_ itin 3;
      for i = 0 to 3 do
	assert ((get_ itin i) = 3 - i)
      done;
      (* make sure we can't unroll an itin which hasn't been completely filled in *)

  )

end;;
