(* 
   dbplot colx coly

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

let usage = "dbcoladd colx coly"
  
let colx = Sys.argv.(1) 
and coly = Sys.argv.(2)

let db = DB.openfile ()
    
let outchan = open_out "/dev/stdout"

let x =  Array.map float_of_string (DB.data_column db colx)
and y =  Array.map float_of_string (DB.data_column db coly)

let _ =
let g = P.init P.X in

P.box g;
P.xlabel g colx;
P.ylabel g coly;
  
P.pen 1;
P.xy g ~style:P.Linespoints x y;
