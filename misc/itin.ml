(* todo:
   1. wrapping of max_int in counter needs to be taken care of 
*)

open Misc
open Larray
open Circbuf

module type Itinerary_t = 
sig
  exception Itin_size_not_set
  type t   
  val make_ : itinsize:int -> graphsize:int -> t
  val length_ : t -> int
  val maxlength_ : t -> int
  val addplace_ : t -> int -> unit
  val get_ : t -> int -> int           (* get an entry in the itinerary, specified by relative age *)
  val hops_to_place_ : t -> int -> int (* throws Failure if place not in itin *)
  val has_place_ : t -> int -> bool   
  val sub_ : t -> int -> t             (* sub itin up to place specified by int *)
  val unroll_ : t -> t                 
  val shorten_ : aux:t -> main:t -> t  
  val splice_ : leftitin:t -> rightitin:t -> int -> t (* replaces rightitin's itinerary upto given place with leftitin's *)
  val equal_ : t -> t -> bool          (* semantic equality *)
  val print_ : t -> int -> unit
end;;

module  Itinerary : Itinerary_t = 
struct
  
  exception Itin_size_not_set
  
  type t =  {cbuf: int CircBuf.circbuf_t; (* head points to last written entry *)
	     graphsize: int; (* number of nodes in the graph over which this itinerary goes *)
	     arr: int array; (* mapping of place to 'time' (from counter) visited. max_int if never visited *)
	     mutable counter: int
	    }

  let last_visit_of_place__ = ref (Array.make 0 max_int) (* to avoid allocating each time in unroll_ *)
    
  let make_ ~itinsize ~graphsize = {cbuf=CircBuf.make_ itinsize ;
				      graphsize=graphsize;
				      arr=Array.make graphsize (max_int);
			              counter=0
				     }

  let length_ itin = CircBuf.length_ itin.cbuf
  let maxlength_ itin = CircBuf.maxlength_ itin.cbuf

  let addplace_ itin place = (
    if (place >= itin.graphsize) then raise (Failure "Itinerary.addplace_ : place bigger than graphsize");
    CircBuf.push_ itin.cbuf place;
    itin.arr.(place) <- itin.counter;
			       
    if (itin.counter == max_int - 1) then 
      failwith "Itinerary.hops_to_place_ : Counter wrapped. Time to implement wrapping and test ..";
    itin.counter <- itin.counter + 1
  )
			       


	
  let get_ itin offset = 
    try CircBuf.get_ itin.cbuf offset with 
	Invalid_argument "Circbuf.get_ : No value at this index" -> raise (Invalid_argument "Itinerary.get_ : No value at this index")
      | Invalid_argument "Circbuf.get_ : Out-of-bounds" -> raise (Invalid_argument "Itinerary.get_ : Out-of-bounds")





  let hops_to_place_ itin place = (
    if itin.arr.(place) == max_int then 
      failwith "Itinerary.hops_to_place_ : place has never been visited"
    else
      let h = itin.counter - itin.arr.(place)  in
	if h > CircBuf.maxlength_ itin.cbuf
	then 
	  failwith "Itinerary.hops_to_place_ : place visited, but out of itinerary"
	else 
	  h - 1 (* -1 so that the last node in itinerary is 0 hops away *)
  )

  let has_place_ itin place = 
    try (ignore (hops_to_place_ itin place); true ) with
	_ -> false
    
  let print_ itin l = (
    for i = 0 to l - 1 do
      Printf.printf "%d " (get_ itin i)
    done;
    Printf.printf "[maxlength %d]" (maxlength_ itin);
    Printf.printf "\n"; flush stdout
  )

  
  let splice_ ~leftitin ~rightitin p = (
    assert (leftitin.graphsize = rightitin.graphsize);

    let l1 = hops_to_place_ leftitin p in
    let l2 = (length_ rightitin) - (hops_to_place_ rightitin p) in
    let newitin = make_ ~itinsize:(l1 + l2)  ~graphsize:leftitin.graphsize in

      for i = (length_ rightitin) - 1 downto (hops_to_place_ rightitin p) do
	addplace_ newitin (get_ rightitin i)
      done;

      for i = (l1 - 1) downto 0 do
	addplace_ newitin (get_ leftitin i)
      done;

      assert ((get_ newitin (length_ newitin - 1)) =  (get_ rightitin (length_ rightitin - 1)));

      newitin;
  )

  (* shortens main using aux *)
  let shorten_ ~aux ~main = (
    assert (aux.graphsize = main.graphsize);
    
    (* xxx/canoptimize could compare lengths and iterate over shortest of two itineraries *)
    let opt_gain = ref 0 in
    let opt_place = ref None in
      for i = 0 to length_ main - 1 do

	let p = get_ main i in begin
	  try (
	    let h1 = hops_to_place_ aux p 
	    and h2 = hops_to_place_ main p in
	      if (h2 - h1) > !opt_gain then (
		opt_gain := h2 - h1;
		opt_place := Some p
	      )
	  ) with
	      Failure "Itinerary.hops_to_place_ : place has never been visited" 
	    | Failure "Itinerary.hops_to_place_ : place visited, but out of itinerary" -> ();
	  end
      done;
      match !opt_place with
	  None -> main
	| Some integer -> splice_ aux main integer
  )

  let sub_ itin target = (

      let index = hops_to_place_ itin target + 1 in
	{cbuf = CircBuf.sub_ itin.cbuf index;
	 graphsize = itin.graphsize;
	 arr = itin.arr;
	 counter = itin.counter}
  )
      

  let unroll_ itin  = (
    
    (* last_visit_of_place__.(n) = how long ago we last visited place n in the graph  (max_int if never visited)  *)
    if (Array.length !last_visit_of_place__ <> itin.graphsize) then 
      last_visit_of_place__ := Array.make itin.graphsize max_int
    else
      (* to avoid reallocs, reuse the same one across invocations when graphsize doesn't change *)
      ArrayLabels.fill !last_visit_of_place__ ~pos:0 ~len:itin.graphsize  max_int;
    
    (* forward-walk list and keep time of first visit through place *)
    (* xxx/slow since we always will iterate over whole list *)
    CircBuf.iteri_ (fun i place -> 
		      try (
			if (!last_visit_of_place__.(place) = max_int) then !last_visit_of_place__.(place) <- i;
		      ) with Invalid_argument "Array.get" -> 
			raise (Invalid_argument "Itinerary.unroll_: itin contained place that was bigger than graphsize")
		   ) itin.cbuf; 
    
    (* reverse-walk list and each time you encounter a place that's not the first encounter noted above,  *)
    (* make the shortcut. *)
    let larr = LinkedArray.make_ (CircBuf.toarray_ itin.cbuf) in (* larr.(0) will be most recently visited place *)
      
    let rec revwalk t = (  
      (* recursion over t:int offset going backward in time *)

      let place = LinkedArray.get_ larr t in
      let most_recent_visit = !last_visit_of_place__.(place) in
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
      if (length_ itin) = 0 then 
	make_ ~itinsize:0 ~graphsize:itin.graphsize
      else (
	
	revwalk (CircBuf.length_ itin.cbuf - 1); 
	let newcb = CircBuf.fromarray_ (LinkedArray.toarray_ larr) in
	let unroll_len = CircBuf.length_ itin.cbuf in
	let newarr = Array.make itin.graphsize max_int in
	  CircBuf.iteri_ (fun i place ->  newarr.(place) <- unroll_len - i - 1) newcb;
	  
	  (* xxx/slow 2 allocations here. Could make a CircBuf.fromlinkedarray that skips one alloc *)
	  {cbuf=newcb;
	   arr=newarr;
	   counter=unroll_len;
	   graphsize=itin.graphsize}
      )
  )		 
			    
  let equal_ it1 it2 = (
    let module M = struct type hops = Hops of int | Never | Out end in
    let hops it p = (
      try M.Hops (hops_to_place_ it p) with
	  Failure "Itinerary.hops_to_place_ : place visited, but out of itinerary" -> M.Out
	| Failure "Itinerary.hops_to_place_ : place has never been visited" -> M.Never
    ) 
    in
      (length_ it1 = length_ it2) &&
      (it1.graphsize = it2.graphsize) &&
      (CircBuf.equal_ it1.cbuf it2.cbuf) &&
      let rec different i = 
	i <  (it1.graphsize) &&  (((hops it1 i) <> (hops it2 i)) || different (i + 1))
      in not (different 0)
			  
  )
      

end;;
