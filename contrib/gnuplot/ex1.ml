(* 	$Id$	 *)
(* From the Matlab plotting examples at
   http://www.indiana.edu/~statmath/math/matlab/plotting/
*)

(* to run:
   ocaml /usr/lib/ocaml/unix.cma  /usr/lib/ocaml/bigarray.cma gnuplot.cmo
   gnuplotBig.cmo ex1.ml 
*)

module P = GnuplotBig.GnuplotBigarrayFortran

let () =
  print_string "first\n";
  let g = P.init ~xsize:500. ~ysize:300. P.X in
  P.box g;
  P.fx g sin (-10.) 10.;
  print_string "open\n";
  P.close g;
  print_string "close\n"


let () =
  let a = -6.
  and b = 6. in
  let g = P.init P.X in
  P.env g a b (-2.) 2.;
  P.fx g cos a b;
  P.pen 2;
  P.fx g (fun x -> 1. -. x**2. /. 2.) a b;
  P.pen 3;
  P.fx g (fun x -> 1. -. x**2. /. 2. +. x**4. /. 24.) a b;
  P.close g

let () =
  let g = P.init P.X in
  P.box g;
  P.xy_param g (fun t -> (t *. cos t, t *. sin t)) (0.) 10.;
  P.close g
