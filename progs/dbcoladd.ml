(* 
   dbaddall col1 .. coln newcol

   Reads a jdb file from stdin, adds columns col1 through coln into a new
   column named newcol, writes new jdb to stdout.

   Bugs: not robust to misformed files 
   (e.g, different # cols at different lines)

   Compile:
   pushd ~/work/caml &&
   ocamlc -I misc -I progs str.cma misc/misc.ml misc/mods.ml progs/dbfile.ml  progs/dbcoladd.ml  -o progs/dbcoladd
   && popd

*)

open Misc

module DB = Dbfile.DBFileReader

let usage = "dbcoladd col1 .. coln newcol"
  
let colnames = 
  Array.sub Sys.argv 1 ((Array.length Sys.argv) - 2)

let newcolname = Sys.argv.((Array.length Sys.argv) - 1)

let db = DB.openfile ()
    
let outchan = open_out "/dev/stdout"

let cols_to_add = 
  Array.map 
    (fun colname -> 
      Array.map 
      (fun s -> float_of_string s) (DB.data_column db colname))
    colnames


let cols_added_floats = 
  Array.init (Array.length cols_to_add.(0))
    (fun row ->
      let sum = ref 0.0 in 
      for i = 0 to (Array.length colnames - 1) do
	sum := !sum +. cols_to_add.(i).(row)
      done;
      !sum) 

let cols_added_strings = 
  Array.map (fun s -> (Printf.sprintf "%.2f" s)) cols_added_floats 

let _ = 
  DB.new_column db newcolname cols_added_strings;
  DB.writefile db;
