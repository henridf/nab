module P = Gnuplot.GnuplotBigarrayFortran

let () =
  let f u v =
    let r = sqrt(u *. u +. v *. v) in sin(r) /. r in
  let g = P.init P.X in
  P.box3 g;
  P.fxy g f (-10.) 10. (-10.) 10.;
  P.close g

let () =
  let sq t = t *. t in
  let f x y =
    let x2 = x *. x
    and y2 = y *. y in
    3. *. sq(1. -. x) *. exp(-. x2 -. sq(y +. 1.)) -.
      10. *. (x/.5. -. x**3. -. y**5.) *. exp(-. x2 -. y2) -.
      exp(-. sq(x+.1.) -. y2) /. 3. in
  let g = P.init P.X in
  P.box3 g;
  P.fxy g f (-2.5) 2.5 (-2.5) 2.5;
  P.close g
