open Misc

module type CircBuffer_t = 
  sig 
    type 'a circbuf
    val create_ : int -> 'a -> 'a circbuf
    val length_ : 'a circbuf -> int
    val get_ : 'a circbuf -> int -> 'a (* offset counts backward from latest element inserted *)
    val push_ : 'a circbuf -> 'a -> unit
    val test_ : unit -> unit
    val toarray_ : 'a circbuf -> 'a array (* array.(0) will be most recently pushed item *)
  end ;;

module CircBuffer : CircBuffer_t = 
  struct 
    type 'a circbuf = {buf : 'a array;
		       mutable head : int}
			
    let create_ size item = (
      if size <= 0   then failwith "CircBuffer.create_ : size must be > 0";
      {buf = (Array.create size item); head=0}
    )
			      
    let length_ cbuf = Array.length cbuf.buf
			 
    let abs2rel cbuf abs = (
      if abs >= cbuf.head then abs - cbuf.head else 
	length_ cbuf - cbuf.head + abs 
    )
			     
    (* rel is a relative offset backwards *)
    let rel2abs cbuf rel = (
      let l = length_ cbuf in
	if rel >=  l then failwith (Printf.sprintf "Itinerary.rel2abs_ : rel. offset (%d) greater than length (%d)" rel l);
	(cbuf.head + rel) mod l 
    )
			     
    let push_ cbuf item = (
      if cbuf.head = 0 then cbuf.head <- (Array.length cbuf.buf - 1) else cbuf.head <- cbuf.head - 1;
      cbuf.buf.(cbuf.head) <- item
    )
			    
    let get_ cbuf offset = cbuf.buf.(rel2abs cbuf offset)
			     
    (* Iterate, going from the head backward.
       Index i passed to f is the relative offset into array
    *)
    let iteri_ f cbuf = (
      let a2r = abs2rel cbuf in
	for i = cbuf.head to (length_ cbuf - 1) do f (a2r i) (Array.unsafe_get cbuf.buf i) done;
	for i = 0 to cbuf.head - 1  do f (a2r i) (Array.unsafe_get cbuf.buf i) done;
    )
			  
    let toarray_ cbuf = (
      let arr = Array.create (length_ cbuf) (get_ cbuf 0) in
	Array.blit cbuf.buf cbuf.head arr 0 (Array.length cbuf.buf - cbuf.head);
	Array.blit cbuf.buf 0 arr (Array.length cbuf.buf - cbuf.head) cbuf.head;
	arr
    )


			  
    let test_ () = ( 
      let cb = create_ 5 1.0 in
	assert (length_ cb = 5);
	  for i = 0 to 4 do 
	    push_ cb (i2f i);
	    assert ((get_ cb 0) = (i2f i));
	  done;
	assert (length_ cb = 5);
	assert (toarray_ cb = [|4.0; 3.0; 2.0; 1.0; 0.0|]);
	  
	  for i = 0 to 4 do 
	    assert ((get_ cb i) = i2f (4 - i))
	  done;
	
	for i = 0 to 2 do 
	  push_ cb (i2f (i + 5));
	done;
	assert (toarray_ cb = [| 7.0; 6.0; 5.0; 4.0; 3.0 |]);
	(* internal representation = [| 4.0; 3.0; 7.0; 6.0; 5.0 |]);*)
	(*  head                                   ^^^              *)
	assert ((get_ cb 0) = 7.0);
	assert ((get_ cb 1) = 6.0);
	assert ((get_ cb 2) = 5.0);
	assert ((get_ cb 3) = 4.0);
	assert ((get_ cb 4) = 3.0);

	let relcheck = ref 0 and valcheck = ref 7 in
	let iterichecker relindex value = (
	  assert (relindex =  !relcheck);
	  assert (f2i value =  !valcheck);
	  incr relcheck;
	  decr valcheck;
	) in
	  iteri_ iterichecker cb;
    )
		     
  end;;

CircBuffer.test_ ();;
