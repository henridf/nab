(* 
   dbplot colx coly1 coly2 ...  plotfile

   Reads a jdb file from stdin, and makes a plot with colx as x values and
   coly as y values.

   Bugs: not robust to misformed files 
   (e.g, different # cols at different lines)

   Compile:
   pushd ~/work/caml &&   ocamlc -I misc -I progs -I contrib/gnuplot unix.cma
   str.cma misc/misc.ml misc/mods.ml   contrib/gnuplot/gnuplot.cma
   progs/dbfile.ml  progs/dbplot.ml  -o progs/dbplot   && popd

*)

open Misc

module DB = Dbfile.DBFileReader
module P = Gnuplot.GnuplotArray

let usage = "dbplot colx coly1 coly2 ...  plotfile"
  
let colx = Sys.argv.(1)
and ycolnames =   Array.sub Sys.argv 2 ((Array.length Sys.argv) - 3)
and outfilename = Sys.argv.((Array.length Sys.argv) - 1)

let db = DB.openfile ()
    


let _ =
    let g = P.init  (P.device_of_filename outfilename) in

  P.box g;

  P.xlabel g "sinks";
  P.ylabel g "packets";

  let x =  Array.map float_of_string (DB.data_column db colx)   in

  for i = 0 to Array.length ycolnames - 1 do
    let y =  Array.map float_of_string (DB.data_column db ycolnames.(i))
    in
    P.pen (i + 1); (* start with pen 1 because pen 0 is dashed*)
    P.xy g ~style:P.Linespoints ~label:ycolnames.(i) x y;
  done;
  P.close g

