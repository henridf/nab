open Misc

module type CircBuf_t = 
sig 
  type 'a circbuf_t

  val make_ : int -> 'a circbuf_t
  val length_ : 'a circbuf_t -> int
  val maxlength_ : 'a circbuf_t -> int
  val get_ : 'a circbuf_t -> int -> 'a              (* offset counts backward from latest element inserted *)
  val push_ : 'a circbuf_t -> 'a -> unit
  val iter_ : ('a -> unit) -> 'a circbuf_t -> unit         
  val iteri_ : (int -> 'a -> unit) -> 'a circbuf_t -> unit
  val fromarray_ : 'a array -> 'a circbuf_t         (* head will be at array.(0) *)
  val toarray_ : 'a circbuf_t -> 'a array           (* array.(0) will be most recently pushed item *)
  val sub_ : 'a circbuf_t -> int -> 'a circbuf_t    (* sub-circbuf with i first elements *)
  val equal_ : 'a circbuf_t -> 'a circbuf_t -> bool (* semantic equality *)
  val test_ : unit -> unit
end ;;


module CircBuf : CircBuf_t = 
struct 
  type 'a circbuf_t = {buf : 'a option array;
		       mutable head : int}
			
  let make_ size = (
    if size < 0   then raise (Invalid_argument "CircBuf.make_ : size must be > 0");
    {buf = (Array.make size None); head=0}
  )
		     


  let maxlength_ cbuf = Array.length cbuf.buf
		       
  let abs2rel__ cbuf abs = (
    if abs >= cbuf.head then abs - cbuf.head else 
      maxlength_ cbuf - cbuf.head + abs 
  )
			   
  (* rel is a relative offset backwards *)
  let rel2abs__ cbuf rel = (
    let l = maxlength_ cbuf in
      if rel >=  l then raise (Invalid_argument (Printf.sprintf "CircBuf.rel2abs__ : rel. offset (%d) greater than maxlength (%d)" rel l));
      if l = 0 then 0 else 
      (cbuf.head + rel) mod l 
  )
			   
  let push_ cbuf item = (
    if maxlength_ cbuf = 0 then raise (Invalid_argument "CircBuf.push_");
    if cbuf.head = 0 then cbuf.head <- (Array.length cbuf.buf - 1) else cbuf.head <- cbuf.head - 1;
    cbuf.buf.(cbuf.head) <- Some item
  )
			  
  let get_ cbuf offset = (
    let res = 
      try cbuf.buf.(rel2abs__ cbuf offset) with Invalid_argument "Array.get" -> raise (Invalid_argument "CircBuf.get_ : Out-of-bounds")
    in
      match res with 
	  None -> raise (Invalid_argument "Circbuf.get_ : No value at this index");
	| Some v -> v
  )

  let length_ cbuf = (
    if maxlength_ cbuf = 0 then 0 
    else
      let i = rel2abs__ cbuf (maxlength_ cbuf - 1) in
	if cbuf.buf.(i) <> None then
	  maxlength_ cbuf (* we've already wrapped, so cbuf is filled *)
	else 
	  if cbuf.head = 0 then 0 else (maxlength_ cbuf) - cbuf.head
  )

  (* Iterate, going from the head backward.
     Index i passed to f is the relative offset into array  *)
  let iteri_ f cbuf = (
    for i = 0 to (length_ cbuf - 1) do
      f i (get_ cbuf i)
    done
  )
			
  (* Iterate, going from the head backward.*)
  let iter_ f cbuf = (
    for i = 0 to (length_ cbuf - 1) do
      f  (get_ cbuf i)
    done
  )

  let toarray_ cbuf = (
    let arr = Array.make (length_ cbuf) None in
      if (length_ cbuf < maxlength_ cbuf) then 
	ArrayLabels.blit ~src:cbuf.buf ~src_pos:cbuf.head ~dst:arr ~dst_pos:0 ~len:(length_ cbuf)
      else (
	ArrayLabels.blit ~src:cbuf.buf ~src_pos:cbuf.head ~dst:arr ~dst_pos:0 ~len:(maxlength_ cbuf - cbuf.head);
	ArrayLabels.blit ~src:cbuf.buf ~src_pos:0 ~dst:arr ~dst_pos:(maxlength_ cbuf - cbuf.head) ~len:cbuf.head;
      );
      Array.map (fun x -> match x with 
		     None -> raise Impossible_Case
		   | Some v -> v
		) arr
  )

  let fromarray_ arr = {buf = Array.map (fun x -> Some x) arr; 
			head = 0}

  let equal_ cb1 cb2 = 
    (length_ cb1 = length_ cb2) && (
      let rec different i = 
	i <  (length_ cb1) &&  (((get_ cb1 i) <> (get_ cb2 i)) || different (i + 1))
      in not (different 0)
    )

  let sub_ cb size = (
    if size > length_ cb then raise (Invalid_argument "CircBuf.sub_ : out-of-bounds");
    let newcb = make_ size in
      for i = (size - 1) downto 0 do
	push_ newcb (get_ cb i)
      done;
      newcb
  )
      


end;;

