(* Illustrate a simple animation displaying the Taylor expansion of sin *)
module P = Gnuplot.GnuplotBigarrayFortran

let device = ref P.X
let specs = [
  ("-f", Arg.String(fun fn -> device := P.device_of_filename fn),
   "specify the filename in which to save the plot")
]
let () = Arg.parse specs (fun _ -> raise (Arg.Bad "no anonymous option")) ""

let usleep t = ignore(Unix.select [] [] [] t)

let tsin d =
  (* Compute the Taylor expansion of sin of order 2d+1 *)
  let d = max d 0 in
  let fac = ref 1.
  and s = ref 1. in
  let p = Array.init (d+1)
            (fun i ->
               let c = !s /. !fac in
               let i2 = 2. *. float(i+1) in
               let () =
                 fac := !fac *. i2 *. (i2 +. 1.);
                 s := -. !s in
               c) in
  fun x ->
    (* Horner eval of the poly *)
    let x2 = x *. x
    and y = ref 0. in
    for i = d downto 0 do
      y := p.(i) +. !y *. x2
    done;
    !y *. x


let () =
  let g = P.init !device in
  let b = 9. in
  let f = Array.init 10 tsin in
  P.env g ~xgrid:true 0. b (-2.) 2.;
  P.pen 2;
  P.pen_width 3.;
  P.fx g sin 0. b;
  P.pen 1;
  P.pen_width 1.;
  for i = 0 to Array.length f - 1 do
    P.fx g f.(i) 0. b;
    usleep 0.5;
  done;

  if !device = P.X && Sys.os_type = "Win32" || Sys.os_type = "Cygwin"
  then Unix.sleep 2;
  P.close g
