open Misc

module type LinkedArray_t = 
sig
  type 'a linkedArray_t
  type ngbr_t = None_head | None_tail | Ngbr of int
  val make_ : 'a array -> 'a linkedArray_t
  val toarray_ : 'a linkedArray_t -> 'a array
  val get_ : 'a linkedArray_t -> int -> 'a
  val next_ : 'a linkedArray_t -> int -> ngbr_t (* None_tail means no successor   *)
  val prev_ : 'a linkedArray_t -> int -> ngbr_t (* None_head means no predecessor *)
  val length_ : 'a linkedArray_t -> int
  val copy_ : 'a linkedArray_t -> 'a linkedArray_t
  val connect_ : 'a linkedArray_t -> ngbr_t -> ngbr_t -> unit 
  val test_ : unit -> unit
end;;

module LinkedArray : LinkedArray_t = 
struct
  
  type ngbr_t = None_head | None_tail | Ngbr of int
  type 'a cell_t = {
    content : 'a;
    mutable prev : ngbr_t;
    mutable next : ngbr_t;
  }
		   
  type 'a linkedArray_t = { 
    arr : 'a cell_t array; 
    mutable head : int;
    mutable tail : int;
  }
			    
  let n2i__ = function 
      None_head -> raise (Invalid_argument "LinkedArray.n2i__: cannot convert None_head to int")
    | None_tail -> raise (Invalid_argument "LinkedArray.n2i__: cannot convert None_tail to int")
    | Ngbr i -> i
	
  let get_ larr i = try larr.arr.(i).content with Invalid_argument "Array.get" -> raise (Invalid_argument "LinkedArray.get_")
  let next_ larr i = try larr.arr.(i).next with Invalid_argument "Array.get" -> raise (Invalid_argument "LinkedArray.next_")
  let prev_ larr i = try larr.arr.(i).prev with Invalid_argument "Array.get" -> raise (Invalid_argument "LinkedArray.prev_")

  let make_ arr = (
    let len = (Array.length arr) - 1 in
    let a = Array.mapi (fun i content ->
			  match i with 
			      0 when (len == 0) -> {content=content; prev=None_head; next=None_tail}
			    | 0 -> {content=content; prev=None_head; next=Ngbr 1}
			    | n when (n == len) -> {content=content; prev=Ngbr (len-1); next=None_tail}
			    | n -> {content=content; prev = Ngbr (n-1); next=Ngbr (n+1)}
		       ) arr in
      {arr=a; head=0; tail=Array.length arr - 1}
  )

  let copy_ larr = {arr=Array.copy larr.arr; head = larr.head; tail = larr.tail}

  let length_ larr = (
    if larr.arr = [||] then 0 else 
      let rec _length ind len =  
	if ind == None_tail then (len) else  _length (next_ larr (n2i__ ind)) (len + 1)
      in
	_length (Ngbr larr.head) 0;
  )




  let toarray_ larr = (
    if length_ larr = 0 then [||] else 
      let arr = Array.make (length_ larr) (get_ larr 0) in
      let rec _toarray  arrindex = function
	  None_tail -> ()
	| n -> 
	    arr.(arrindex) <- (get_ larr (n2i__ n));
	    _toarray (arrindex + 1) (next_ larr (n2i__ n)) 
      in
	_toarray 0 (Ngbr larr.head);
	arr;
  )

  let connect_ larr ngbr1 ngbr2 = (

    match (ngbr1, ngbr2) with
	(Ngbr n1, Ngbr n2) when ((n1 < 0) || (n2 < 0)) -> 
	  raise (Invalid_argument "LinkedArray.connect : invalid neigbor (is negative)")

      | (Ngbr n1, Ngbr n2) when ((n1 >= length_ larr) || (n2 >= length_ larr)) -> 
	  raise (Invalid_argument "LinkedArray.connect : invalid neigbor (is too big)")

      | (Ngbr n1, Ngbr n2) when (n1 = n2) -> 
	  raise (Invalid_argument "LinkedArray.connect : cannot connect an entry to itself")

      | (Ngbr left, Ngbr right) when (left < right) ->
	  larr.arr.(left) <- {larr.arr.(left) with next = Ngbr right};
	  larr.arr.(right) <- {larr.arr.(right) with prev = Ngbr left};
	  
      | (Ngbr right, Ngbr left) when (left < right) ->
	  larr.arr.(left) <- {larr.arr.(left) with next = Ngbr right};
	  larr.arr.(right) <- {larr.arr.(right) with prev = Ngbr left};

      | (None_head, Ngbr newhead) | (Ngbr newhead, None_head) ->
	  larr.arr.(newhead) <- {larr.arr.(newhead) with prev = None_head};
	  larr.head <- newhead

      | (Ngbr newtail, None_tail) | (None_tail, Ngbr newtail) ->
	  larr.arr.(newtail) <- {larr.arr.(newtail) with next = None_tail};
	  larr.tail <- newtail

      | (None_head, None_tail) | (None_tail, None_head) ->
	  larr.arr.(larr.head) <- {larr.arr.(larr.head) with next = None_tail};
	  larr.tail <- larr.head
	    
      | _ -> raise (Impossible_Case "Larray.connect_")

  )


  let consistency_check__ larr = (

    assert (larr.head <= larr.tail);
    assert (prev_ larr (larr.head) = None_head);
    assert (next_ larr (larr.tail) = None_tail);
    assert (if (larr.head = larr.tail) then (next_ larr (larr.head) = None_tail)
	    else true);
    
    let rec _fw_check = function
	None_tail -> ()
      | n when (next_ larr (n2i__ n)) = None_tail -> ()
      | n -> 
	  let next = (next_ larr (n2i__ n)) in
	    assert (prev_ larr (n2i__ next) = n);
	    _fw_check next
    in
    let rec  _bw_check = function
	None_head -> ()
      | n when (prev_ larr (n2i__ n)) = None_head -> ()
      | n -> 
	  let prev = (prev_ larr (n2i__ n)) in
	    assert (next_ larr (n2i__ prev) = n);
	    _fw_check prev
    in
      _fw_check (Ngbr larr.head);
      _bw_check (Ngbr larr.tail);
  )


  let test_ () = (
    (* connect head to tail, check length = 0 and toarray = [||] *)

    let la = make_ [||] in 
      assert (length_ la = 0);
      assert (toarray_ la = [||]);

      let la = make_ [|1.0|] in 
	assert (length_ la = 1);
	assert (toarray_ la = [|1.0|]);
	consistency_check__ la;

	let la = make_ [| 0.; 1.; 2.; 3.; 4.; 5. |] in 
	  assert (length_ la = 6);
	  assert (toarray_ la = [| 0.; 1.; 2.; 3.; 4.; 5. |]);
	  for i = 0 to 5 do 
	    assert ((get_ la i) = i2f i);
	    assert ((prev_ la i) =  if i = 0 then None_head else Ngbr (i - 1));
	    assert ((next_ la i) = if i = 5 then None_tail else  Ngbr (i + 1));
	  done;
	  consistency_check__ la;

	  let lacp = copy_ la in 
	    connect_ lacp (Ngbr 0) (Ngbr 1);
	    consistency_check__ lacp;
	    connect_ lacp (Ngbr 1) (Ngbr 2);
	    consistency_check__ lacp;
	    connect_ lacp (Ngbr 4) (Ngbr 3);
	    consistency_check__ lacp;
	    connect_ lacp (Ngbr 5) (Ngbr 4);
	    consistency_check__ lacp;
	    assert (lacp = la);
	    connect_ lacp (Ngbr 5) (Ngbr 1);
	    consistency_check__ lacp;
	    assert (length_ lacp = 3);
	    assert (toarray_ lacp = [|0.0; 1.0; 5.0|]);
	    
            (* connect head to tail, check length = 0 and toarray = [||] *)
	    connect_ lacp None_head None_tail;
	    assert (length_ lacp = 1);
	    assert (toarray_ lacp = [|0.0|]);
	    Printf.printf "LinkedArray.test_ : passed \n";
  )
		   
end;;


LinkedArray.test_ ();;

