open Printf
module P = Gnuplot.GnuplotArray

let () =
  let n = 100 in
  let x = Array.create n 0. in
  for i = 0 to n - 1 do
    x.(i) <- Random.float 0.3 +. sin(4. *. float i /. float n)
  done;
  let g = P.init P.X in
  P.box g;
  P.x g x;
  P.close g

let () =
  (* Histogram of binomial distribution *)
  let p = 0.2 (* probability of success of a single trial *)
  and n = 15  (* number of trials *) in
  let x = Array.create (n+1) 0.
  and y = Array.create (n+1) 0. in
  let nf = float n in
  let mean = nf *. p
  and sigma = sqrt(nf *. p *. (1. -. p))
  and nchooseu = ref 1. in
  for i = 0 to n do
    let u = float i in
    x.(i) <- (u -. mean) /. sigma;
    y.(i) <- sigma *. !nchooseu *. p**u *. (1. -. p)**(nf -. u);
    nchooseu := !nchooseu *. (nf -. u) /. (u +. 1.);
  done;
  let pi = 4. *. atan 1. in
  let gauss x = 1. /. sqrt(2. *. pi) *. exp(-.(x**2.)/.2.) in
  let a = (nf -. mean) /. sigma in
  let g = P.init P.X in
  P.box g;
  P.title g (sprintf "Bin(p=%g,n=%i) --> Gauss" p n);
  P.pen_width 2.;
  P.bin g x y;
  P.pen 3;
  P.fx g gauss (-. a) a;
  P.close g
