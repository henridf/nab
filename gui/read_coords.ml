open Graph
open Misc

let gr = ref None
let g () = o2v !gr

let make_graph() = (

let ic = open_in "/home/henri/work/150th/coordinates.txt" in

let re = Str.regexp "[ \t]+" in

gr := Some (Graph.make_ "" 114 Graph.Undirected);

begin
  try 
    while true do
      let arr = Array.of_list (Str.split (Str.regexp "[ \t]+") (input_line ic)) in
      let node = arr.(0) in
	Graph.add_node_ (g()) node;
	let info = Array.map 
	  (fun s -> int_of_string s) 
	  (Array.sub arr 1 8) in
	Graph.setinfo_ (g()) node (Array.to_list info)
      done
  with 
    | End_of_file -> ()
    | e -> raise e;
  end;

  seek_in ic 0;
  
  begin
    try 
      while true do
	let arr = Array.of_list (Str.split (Str.regexp "[ \t]+") (input_line ic)) in
	let node = arr.(0) in
	Array.iteri 
	  (fun i ngbr -> 
	    if i > 8 then (
	      Graph.add_edge_ (g()) node ngbr 1.0;
	    )
	  ) arr
      done
  with 
    | End_of_file -> ()
    | e -> raise e

  end;

)
(*
Check connectedness
  Graph.itern_ (fun src -> 
  Graph.itern_ (fun dst -> 
    Printf.printf "doing %s-%s\n" src dst;
    ignore (Graph.route_dij_ g src dst);
  ) g
) g
*)
