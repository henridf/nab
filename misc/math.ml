
let bigfacto n = 
  let rec tailrec x n = 
    match x with
      | 0 -> n
      | v -> tailrec (x - 1) (Big_int.mult_int_big_int x  n)
  in
  tailrec n Big_int.unit_big_int

(* note that if this is used in perf. intensive manner, may be worthwhile to
   use int32's or int64's in some cases, since bigfacto may be slow *)
let bigbinomial ~pick ~outof = 
  Big_int.div_big_int
    (bigfacto outof)  
    (Big_int.mult_big_int (bigfacto pick)  (bigfacto (outof - pick)))

