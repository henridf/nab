(**
  Run:
  CHANGE sep3 to whatever!!
  pushd /home/henri/work/caml/contrib/gnuplot && 
  ocaml -I /home/henri/work/caml/misc unix.cma bigarray.cma misc.cmo  gnuplot.cmo  inhib.ml  && popd
*)

open Printf
module P = Gnuplot.GnuplotArray

let file_extension = ".eps"

let lines_of_file_nocomments fname = 
  let all_lines = Misc.lines_of_file fname in
  List.filter (fun s -> s.[0] <> '#') all_lines

let line_reader line = 
  Scanf.sscanf line "%f %f %f %f" 
    (fun mean conf_low conf_high sinks -> (mean, conf_low, conf_high, sinks))
    
let line fname  = 
  let strings = lines_of_file_nocomments  ("/home/henri/censwork/exp/results/sep2/processed/"^fname) in
  let values = List.map line_reader strings
  in 
  let x = Array.of_list (List.map (fun (_,_,_,sinks) -> sinks)  values)
  and m = Array.of_list (List.map (fun (mean,_,_,_) -> mean) values)
  and l = Array.of_list (List.map (fun (_,conf_low,_,_) -> conf_low) values)
  and h = Array.of_list (List.map (fun (_,_,conf_high,_) -> conf_high) values)
  in (x, m, l, h)


let () =
  let g = P.init 
(*    P.X*)
    ~xsize:300. ~ysize:250.
    (P.device_of_filename ("/tmp/data"^file_extension)) 
  in
  P.xlabel g "Sinks";
  P.ylabel g "Packets";
  P.box g;
  P.pen 1;
  let (x, m, _, _) = line "opp-5-data-orig.dat" in
  P.xy ~style:P.Linespoints ~label:"DATA orig OPP" g x m;

  P.pen 2;
  let (x, m, _, _) = line "opp-5-data-sent.dat" in
  P.xy ~style:P.Linespoints ~label:"DATA sent OPP" g x m;

  P.pen 3;
  let (x, m, _, _) = line "inhib-1-data-orig.dat" in
  P.xy ~style:P.Linespoints ~label:"DATA orig VOR" g x m;

  P.pen 4;
  let (x, m, _, _) = line "inhib-1-data-sent.dat" in
  P.xy ~style:P.Linespoints ~label:"DATA sent VOR" g x m;
  P.close g;

  let g = P.init 
    (*    P.X*)
    ~xsize:300. ~ysize:250.
    (P.device_of_filename ("/tmp/interest"^file_extension)) 
  in
  (*  P.adv g;*)
  P.xlabel g "Sinks";
  P.ylabel g "Packets";
  P.box g;
  
  P.pen 1;
  let (x, m, _, _) = line "opp-5-interest-orig.dat" in
  P.xy ~style:P.Linespoints ~label:"INT orig OPP" g x m;

  P.pen 2;
  let (x, m, _, _) = line "opp-5-interest-sent.dat" in
  P.xy ~style:P.Linespoints ~label:"INT sent OPP" g x m;

  P.pen 3;
  let (x, m, _, _) = line "inhib-1-interest-orig.dat" in
  P.xy ~style:P.Linespoints ~label:"INT orig VOR" g x m;

  P.pen 4;
  let (x, m, _, _) = line "inhib-1-interest-sent.dat" in
  P.xy ~style:P.Linespoints ~label:"INT sent VOR" g x m;

  P.close g;

  let g = P.init 
    (*    P.X*)
    ~xsize:300. ~ysize:250.
    (P.device_of_filename ("/tmp/delivered"^file_extension) )
  in
  (*  P.adv g;*)
  P.xlabel g "Sinks";
  P.ylabel g "Packets";
  P.box g;
  
  P.pen 1;
  let (x, m, _, _) = line "opp-5-data-sinked.dat" in
  P.xy ~style:P.Linespoints ~label:"DATA deliv OPP" g x m;

  P.pen 2;
  let (x, m, _, _) = line "inhib-1-data-sinked.dat" in
  P.xy ~style:P.Linespoints ~label:"DATA deliv VOR" g x m;

  P.close g;



  ignore (P.border ~which:P.Both ())
