(** Miscellaneous stuff that is useful everywhere **)

(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(**                                                                                     **)
(** Types and Exceptions                                                                **)
(**                                                                                     **)
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

exception Impossible_Case;;


(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(**                                                                                     **)
(** Shorthands                                                                          **)
(**                                                                                     **)
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)



let i2f i = float_of_int i;;
let f2i f = int_of_float f;;

let s2i s = int_of_string s;;
let s2f s = float_of_string s;;

let i2s i = string_of_int i;;
let f2s f = string_of_float f;;

let repeat n f = begin
  let ctr = ref 0 in 
    while (!ctr < n) do f (); incr ctr  done;
end

let largest f1 f2 = 
  if (f1 > f2) then f1 else f2;;

let smallest f1 f2 = 
  if (f1 < f2) then f1 else f2;;

let (+++) (a, b) (c, d) = (a + c, b+ d);;
let (+++.) (a, b) (c, d) = (a +. c, b+. d);;
let (---) (a, b) (c, d) = (a - c, b - d);;
let (---.) (a, b) (c, d) = (a -. c, b -. d);;

let isint x = (x -. floor x) < epsilon_float;;
let round x = if (x -. floor x) < 0.5 then (floor x) else (ceil x);;

(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(**                                                                                     **)
(**  Lists                                                                              **)
(**                                                                                     **)
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

(** Returns the j first elements of thelist **)
let listuntil j thelist = 
  if (j > List.length thelist) then failwith "listuntil" else
    let rec listuntil j left right = 
      match j with 
	  0 -> List.rev left
	| _ -> listuntil (j -1 )  (( List.hd right)::left) (List.tl right)
    in
      listuntil j [] thelist;;

(** Returns the second part of the list starting at element j **)
let listfrom j thelist = List.rev (listuntil (List.length thelist - j) (List.rev thelist));;

let listbetween i j thelist = List.rev (listuntil (j -i ) (List.rev (listuntil j thelist)));;

let cycle_list l = (List.tl l)@[List.hd l] ;;

let listlast l = List.nth l (List.length l - 1);;
  
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(**                                                                                     **)
(**  Arrays                                                                             **)
(**                                                                                     **)
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

let array_between array start finish = (
  if (finish < start) then raise (Failure "array_between: invalid args");
  let len = finish - start + 1 in
    Array.sub array start len;
)

(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)
(**                                                                                     **)
(**  Iterators                                                                          **)
(**                                                                                     **)
(** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **)

let matrix_iter f m = 
  let iter_row r = Array.iter f r in
    Array.iter iter_row m;;
