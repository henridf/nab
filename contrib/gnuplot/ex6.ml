module P = Gnuplot.GnuplotBigarrayC

let () =
  let re_sq u v = (u *. u -. v *. v, 2. *. u *. v, u) in
  let im_sq u v = (u *. u -. v *. v, 2. *. u *. v, v) in
  let g = P.init ~xsize:800. ~ysize:400. ~nxsub:2 P.X in
  P.box3 g;
  P.fxy_param g re_sq (-3.) 3. (-3.) 3.;
  P.adv g;
  P.box3 g;
  P.fxy_param g im_sq (-3.) 3. (-3.) 3.;
  P.close g

let pi = 4. *. atan 1.

let () =
  let torus x0 y0 z0 r0 r1 u v =
    let r = r0 -. r1 *. cos(v) in
    (x0 +. r *. cos(u), y0 +. r *. sin(u), z0 +. r1 *. sin(v)) in
  let torus2 x0 y0 z0 r0 r1 u v =
    let r = r0 -. r1 *. cos(v) in
    (x0 +. r1 *. sin(v), y0 +. r *. cos(u), z0 +. r *. sin(u)) in
  let g = P.init ~xsize:800. ~ysize:400. ~nxsub:2 P.X in
  P.box3 g;
  P.fxy_param g (torus 0. 0. 0. 1. 0.3) 0. (2.*.pi) 0. (2.*.pi);
  P.fxy_param g (torus 2. 0. 0. 1. 0.3) 0. (2.*.pi) 0. (2.*.pi);
  P.pen 2;
  P.fxy_param g (torus2 0. 1. 0. 1. 0.2) 0. (2.*.pi) 0. (2.*.pi);
  P.adv g;
  let moebius u v =
    let r = 2. -. v *. sin(u/.2.) in
    (r *. sin(u), r *. cos(u), v *. cos(u/.2.)) in
  P.box3 g;
  P.pen 1;
  P.fxy_param g moebius 0. (2. *. pi) (-0.25) 0.25;
  P.close g
