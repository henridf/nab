(* Miscellaneous stuff that is useful everywhere **)

open Printf

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(*                                                                                     **)
(* Shorthands                                                                          **)
(*                                                                                     **)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)



let i2f i = float_of_int i
let f2i f = int_of_float f

let s2i s = int_of_string s
let s2f s = float_of_string s

let i2s i = string_of_int i
let f2s f = string_of_float f

let repeat n f = begin
  let ctr = ref 0 in 
    while (!ctr < n) do f (); incr ctr  done;
end

let largest f1 f2 = 
  if (f1 > f2) then f1 else f2

let smallest f1 f2 = 
  if (f1 < f2) then f1 else f2

let (+++) (a, b) (c, d) = (a + c, b+ d)
let (+++.) (a, b) (c, d) = (a +. c, b+. d)
let (---) (a, b) (c, d) = (a - c, b - d)
let (---.) (a, b) (c, d) = (a -. c, b -. d)
let (///) (a, b) c = (a/c, b/c)
let (///.) (a, b) c = (a/.c, b/.c)
 
let pair_i2f (a, b) = (i2f a, i2f b)
let pair_f2i (a, b) = (f2i a, f2i b)

let isint x = (x -. floor x) < epsilon_float
let round x = if (x -. floor x) < 0.5 then (floor x) else (ceil x)

let powi num exp = f2i ((i2f num) ** (i2f exp))
let sign x = if x < 0.0 then (-1.0) else (1.0)
let signi x = if x < 0 then (-1) else (1)

let ispower ~pow ~num = isint ((i2f num) ** (1.0 /. i2f pow))
let issquare = ispower ~pow:2

let ininterval ~num ~left ~right = (num >=left) && num <= right

let norm a = let n = ref 0.0 in Array.iter (fun x -> n := !n +. (x ** 2.0)) a; sqrt !n
let normdot a = let n = ref 0.0 in Array.iter (fun x -> n := !n +. ((i2f x) ** 2.0)) a; sqrt !n

let id () = ()

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(*                                                                                     **)
(*  Lists                                                                              **)
(*                                                                                     **)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

(* Returns the j first elements of thelist **)
let listuntil j thelist = 
  if (j > List.length thelist) then failwith "listuntil" else
    let rec listuntil j left right = 
      match j with 
	  0 -> List.rev left
	| _ -> listuntil (j -1 )  (( List.hd right)::left) (List.tl right)
    in
      listuntil j [] thelist

(* Returns the second part of the list starting at element j **)
let listfrom j thelist = List.rev (listuntil (List.length thelist - j) (List.rev thelist))

let listbetween i j thelist = List.rev (listuntil (j -i ) (List.rev (listuntil j thelist)))

let cycle_list l = (List.tl l)@[List.hd l] 

let listlast l = List.nth l (List.length l - 1)
  
let rnd_from_list l = List.nth l (Random.int (List.length l))
  
let list_same l1 l2 = (List.sort compare l1 = List.sort compare l2) 

let list_without l el = List.filter (fun x -> x <> el) l
			  
let list_unique_elements l = 
  let hash = Hashtbl.create (List.length l) in
    List.iter (fun x -> Hashtbl.remove hash x; Hashtbl.add hash x "") l;
    Hashtbl.fold (fun key value list -> key :: list ) hash []

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(*                                                                                     **)
(*  Arrays                                                                             **)
(*                                                                                     **)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

let array_between array start finish = (
  if (finish < start) then raise (Failure "array_between: invalid args");
  let len = finish - start + 1 in
    Array.sub array start len;
)

let array_f2i a = Array.map (fun x -> f2i x ) a
let array_i2f a = Array.map (fun x -> i2f x ) a

(* copied from array.mli
   val iteri : (int -> 'a -> unit) -> 'a array -> unit 
   val iter : ('a -> unit) -> 'a array -> unit *)
let array_rev_iteri f a = 
  for i = Array.length a - 1 downto 0 do f i (Array.unsafe_get a i) done

let array_rev_iter f a = 
  for i = Array.length a - 1 downto 0 do f (Array.unsafe_get a i) done


(*val array_count_filt : ('a -> bool) -> 'a array -> int = <fun> *)
let array_count_filt f a = (
  let c = ref 0 in 
    Array.iter (fun el -> if (f el) then incr c) a; 
    !c
)

(* val array_count : 'a -> 'a array -> int = <fun> *)
let array_count elt a = array_count_filt (fun x -> x = elt) a


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(*                                                                                     **)
(*  Iterators                                                                          **)
(*                                                                                     **)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

let matrix_iter f m = 
  let iter_row r = Array.iter f r in
    Array.iter iter_row m

    

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(*                                                                                     **)
(*  Options                                                                            **)
(*                                                                                     **)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

let o2v o = function 
    None -> raise (Failure "Misc.o2v : None")
  | Some v -> v


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(*                                                                                     **)
(* Error Handling                                                                      **)
(*                                                                                     **)
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

exception Impossible_Case
exception Break

let equal_or_print a b ~equal ~print =
    if not (equal a b) then (
      print a;
      print b;
      false
    ) else true
	


