(* 
   dbrowaccumall infile outfile

   Takes a file in jdb format, adds all columns, leaving header intact.
   Output therefore has only one line of data.

   Bugs: not robust to misformed files 
   (e.g, different # cols at different lines)

   Compile:
   pushd ~/work/caml &&
   ocamlc -I misc -I progs str.cma misc/misc.ml misc/mods.ml progs/dbfile.ml  progs/dbrowaccumall.ml  -o progs/dbrowaccumall
   && popd

*)

open Misc

module DB = Dbfile.DBFileReader

let usage = "dbrowaccumall infile outfile"
  
let infile = 
  try Sys.argv.(1) with _ -> "/dev/stdin"
let outfile = 
  try Sys.argv.(2) with _ -> "/dev/stdout"
      

let db = DB.openfile ~infile ()

let sum_column n = 
  let column_name = List.nth (DB.header db) n in
  let col = DB.data_column db column_name in
  let floatcol = Array.map float_of_string col in 
  Array.fold_left 
    (fun a b ->  a +. b)  0. floatcol

    
let sum_all_columns = 
  Array.to_list (Array.init (DB.width db)  (fun i -> sum_column i))

let sum_all_columns_string =     
  List.map     
    (fun num ->  Printf.sprintf "%.2f " num)
    sum_all_columns

let outchan = open_out outfile


let _ = 
  let outdb = DB.newdb () in
  DB.new_header outdb (DB.header db);
  DB.add_comments outdb (DB.comment_lines db);
  DB.add_line outdb sum_all_columns_string;
  DB.writefile ~outfile outdb

(*
  (* print headers back to file *)
  List.iter (fun s -> output_string outchan (s^"\n")) (DB.comment_lines db);
  (* print line of summed columns *)
  Array.fold_left 
    (fun _ num -> 
      Printf.fprintf outchan "%.2f " num)
    () 
    sum_all_columns;
  Printf.fprintf outchan "\n";
  close_out outchan
*)
