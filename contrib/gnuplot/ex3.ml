(* 	$Id$	 *)

module P = Gnuplot.GnuplotBigarrayC

let device = ref P.X
let specs = [
  ("-f", Arg.String(fun fn -> device := P.device_of_filename fn),
   "specify the filename in which to save the plot")
]
let () = Arg.parse specs (fun _ -> raise (Arg.Bad "no anonymous option")) ""

let usleep t = ignore(Unix.select [] [] [] t)

let () =
  P.pen_width 1.;
  let g = P.init ~nxsub:2 ~nysub:2 !device in
  P.box g;
  P.fx g ~style:P.Impulses (fun x -> x) 0. 1.;
  P.pen 2;
  P.fx g ~style:P.Linespoints (fun x -> x**2.) 0. 1.;

  P.adv g;
  P.box g;
  P.pen 3;
  P.pen_width 2.;
  P.title g "Spiral";
  P.xy_param g (fun t -> let r = 0.1 *. t in
                (r *. cos t, r *. sin t)) 0. 13.;

  P.adv g;
  P.pen 2;
  P.env g 0. 10. ~ylog:true ~ygrid:true 1e-5 10.;
  P.pen 1;
  P.fx g (fun x -> exp(-. x)) 0.01 10.;

  P.adv g;
  P.env g ~xgrid:true 0. 1. ~ygrid:true 0. 1.;
  List.iter (fun e ->
               P.fx g (fun x -> x**e) 0. 1.;
               if !device = P.X then usleep 0.5;
            ) [0.1; 0.25; 0.5; 1.; 2.; 4.; 10.];

  if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then Unix.sleep 2;
  P.close g
