open Misc

module type LinkedArray_t = 
  sig
    type 'a linkedArray
    type link_t = None | Link of int
    val create_ : 'a array -> 'a linkedArray
    val toarray_ : 'a linkedArray -> 'a array
    val get_ : 'a linkedArray -> int -> 'a
    val next_ : 'a linkedArray -> int -> link_t (* None means no successor   *)
    val prev_ : 'a linkedArray -> int -> link_t (* None means no predecessor *)
    val connect_ : 'a linkedArray -> int -> int -> unit
    val test_ : unit -> unit
  end;;

module LinkedArray : LinkedArray_t = 
  struct
    
    type link_t = None | Link of int
    type 'a cell = {
      content : 'a;
      mutable prev : link_t;
      mutable next : link_t;
    }

    type 'a linkedArray = 'a cell array

    let l2i = function 
	None -> failwith "Cannot convert None to int"
      | Link i -> i

      
    let create_ arr = (
      let len = (Array.length arr) - 1 in
      Array.mapi (fun i content ->
		    match i with 
			0 when (len == 0) -> {content=content; prev=None; next=None}
		      | 0 -> {content=content; prev=None; next=Link 1}
		      | n when (n == len) -> {content=content; prev=Link (len-1); next=None}
		      | n -> {content=content; prev = Link (n-1); next=Link (n+1)}
		 ) arr
    )

    let length_ larr = (
      if larr = [||] then 0 else 
	let rec _length ind len =  
	  if ind == None then (len) else  _length larr.(l2i ind).next (len + 1)
	in
	  _length (Link 0) 0;
    )


    let get_ larr i = larr.(i).content
    let next_ larr i = larr.(i).next
    let prev_ larr i = larr.(i).prev


    let toarray_ larr = (
      if Array.length larr = 0 then [||] else 
	let arr = Array.create (length_ larr) larr.(0).content in
	let rec _toarray  arrindex = function
	    None -> ()
	  | n -> 
	      arr.(arrindex) <- larr.(l2i n).content;
	      _toarray (arrindex + 1) (next_ larr (l2i n)) 
	in
	  _toarray 0 (Link 0);
	  arr;
    )


    let connect_ larr i1 i2 = (
      let min = 0 and max = (Array.length larr) - 1 in (
	  if not ((ininterval i1 min max) &&  (ininterval i2 min max)) then (
	    failwith (Printf.sprintf 
			"LinkedArray.connect_: Cannot connect %d and %d in linkedArray of length %d"
			i1 i2 (Array.length larr)
		     )
	  );
	  if (i1 = i2) then failwith "LinkedArray.connect_: Cannot connect an entry to itself";
	);
	
	let left = smallest i1 i2 and right = largest i1 i2 in
	  larr.(left) <- {larr.(left) with next = Link right};
	larr.(right) <- {larr.(right) with prev = Link left};
    )


    let consistency_check_ larr = (
      let rec _fw_check = function
	  None -> ()
	| n when (next_ larr (l2i n)) = None -> ()
	| n -> 
	    let next = (next_ larr (l2i n)) in
	      assert (prev_ larr (l2i next) = n);
	      _fw_check next
      in
      let rec  _bw_check = function
	  None -> ()
	| n when (prev_ larr (l2i n)) = None -> ()
	| n -> 
	    let prev = (prev_ larr (l2i n)) in
	      assert (next_ larr (l2i prev) = n);
	      _fw_check prev
      in
	_fw_check (Link 0);
	_bw_check (Link (Array.length larr - 1));
    )

    let test_ () = (

      let la = create_ [||] in 
	  assert (length_ la = 0);
	  assert (toarray_ la = [||]);

      let la = create_ [|1.0|] in 
	  assert (length_ la = 1);
	  assert (toarray_ la = [|1.0|]);
	  consistency_check_ la;

      let la = create_ [| 0.; 1.; 2.; 3.; 4.; 5. |] in 
	  assert (length_ la = 6);
	  assert (toarray_ la = [| 0.; 1.; 2.; 3.; 4.; 5. |]);
	  for i = 0 to 5 do 
	    assert ((get_ la i) = i2f i);
	    assert ((prev_ la i) =  if i = 0 then None else Link (i - 1));
	    assert ((next_ la i) = if i = 5 then None else  Link (i + 1));
	  done;
	  consistency_check_ la;

	  let lacp = Array.copy la in 
	    connect_ la 0 1;
	    consistency_check_ lacp;
	    connect_ la 1 2;
	    consistency_check_ lacp;
	    connect_ la 4 3;
	    consistency_check_ lacp;
	    connect_ la 5 4;
	    consistency_check_ lacp;
	    assert (lacp = la);
	    connect_ la 5 1;
	    consistency_check_ lacp;
	    assert (length_ la = 3);
	    assert (toarray_ la = [|0.0; 1.0; 5.0|])
    )
	  
  end;;
 

LinkedArray.test_ ();;

