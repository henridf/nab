(* 
   draw_emsim_nodes x [true|false] file1 file2 file2

   x : square side of surface
   [true|false] : draw connectivity or not
   fileN : files containing positions 

   Assumes input file(s) with lines like:
   (like *.dat files output by emstar_topot)

   3.24, 49.63
   32.78, 37.67
   16.27, 2.99
   42.73, 44.94
   ...

   
   ocamlc -I misc /usr/lib/ocaml/graphics.cma /usr/lib/ocaml/str.cma
   misc/misc.cmo misc/coord.cmo misc/ler_graphics.cmo
   progs/draw_emsim_nodes.ml -o bin/draw_emsim_nodes

   ./bin/draw_emsim_nodes 43 false  ../../censwork/exp/topologies/opp/{sinks,sources}-13.dat


*)

let usage = "draw_emsim_nodes x [true|false] file1 file2 file2"

let _ = 
  if Array.length Sys.argv < 4 then 
    failwith usage
let size = float_of_string Sys.argv.(1) 
let draw_conn = 
  match Sys.argv.(2) with
    | "true" | "True" -> true
    | "false" | "False" -> false
    | other -> failwith (other^": Invalid value for boolean draw_conn")
	
let radio_range = 8. /. size

let read_line l = 
  let strings = 
    Str.split (Str.regexp ", ") l in 
  if (List.length strings) <> 2 then (
    Printf.fprintf stderr "ERROR: \n Read a line of unknown format: %s\n" l;
    exit (-1)
  );
  match strings with 
    | [x ; y] ->
	(float_of_string x, float_of_string y)
    | _ -> raise (Misc.Impossible_Case "") (* we have checked length above*)



let normalize_positions arr = 
  Array.map (fun (x, y) -> (x /. size, y /. size)) arr

let get_node_positions() = 
  (* hack: nodes are labelled starting at 0 by ler_graphics, so we add a fake
     node to offset and match emsim*)
  let pos = ref [|(0.0, 0.0)|] in
  for i = 3 to (Array.length Sys.argv) - 1 do
    let filename = Sys.argv.(i) in
    pos := Array.append !pos (Misc.file_map_array filename read_line)
  done;
  normalize_positions !pos

let draw_node_neighbors nindex arr = 
  let npos = arr.(nindex) in
  let dist_to_node n = 
    Coord.dist_sq npos n in
  Array.iter 
    (fun n -> 
      if (dist_to_node n < (radio_range ** 2.0)) then
	Ler_graphics.ler_draw_segment [|npos; n|]
    ) arr


let _ = 



  Ler_graphics.init_gfx();
  let arr = get_node_positions() in
  Ler_graphics.draw_and_label_nodes arr;
  if draw_conn then begin
    for i = 1 to (Array.length arr) - 1 do
      draw_node_neighbors i arr
    done;
  end;
  Misc.wait_for_line()
