(* 
   dbplotmulti colx file1 file2 ...  plotfile

   Reads one or more jdb "stat" files, and makes a plot with all lines, showing
   conf intervals. "colx" is the name of the x data column (ie "nsinks"
   below), should be present  in all files.
   By "stat" files, we mean a file likely made with dbmultistats looking like e.g:

   #h nsinks mean stddev pct_rsd conf_range conf_low conf_high conf_pct sum sum_squared min max n
   1	4660	572.96	12.295	317.33	4342.7	4977.3	0.95	69900	3.3033e+08	4000	5500	15
   2	6896.7	2529.4	36.676	1400.9	5495.8	8297.6	0.95	1.0345e+05	8.0305e+08	2723	9100	15
   3	9911.1	4661.4	47.032	2581.6	7329.5	12493	0.95	1.4867e+05	1.7777e+09	3225	14600	15

   Bugs: not robust to misformed files 
   (e.g, different # cols at different lines)

   Compile:
   pushd ~/work/caml &&   ocamlc -I misc -I progs -I contrib/gnuplot unix.cma
   str.cma misc/misc.ml misc/mods.ml   contrib/gnuplot/gnuplot.cma
   progs/dbfile.ml  progs/dbplotmulti.ml  -o progs/dbplotmulti   && popd
*)

open Misc
  
module DB = Dbfile.DBFileReader
module P = Gnuplot.GnuplotArray
  
let usage = "dbplotmulti colx file1 file2 ...  plotfile"
  
let colx = Sys.argv.(1)
let infiles =  Array.sub Sys.argv 2 ((Array.length Sys.argv) - 3)
and outfilename = Sys.argv.((Array.length Sys.argv) - 1)

let _ =
    let g = P.init  (P.device_of_filename outfilename) in

  P.box g;

  P.xlabel g "sinks";
  P.ylabel g "packets";


  for i = 0 to Array.length infiles - 1 do
    let db = DB.openfile ~infile:infiles.(i) () in
    let x =  Array.map float_of_string (DB.data_column db colx)   in
    let y =  Array.map float_of_string (DB.data_column db "mean")
    in
    P.pen (i + 1); (* start with pen 1 because pen 0 is dashed*)
    P.xy g ~style:P.Linespoints ~label:infiles.(i) x y;
  done;
  P.close g

