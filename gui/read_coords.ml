open Graph
open Coord
open Misc

let gr = ref None
let g () = o2v !gr


let make_graph() = (

  let ic = open_in "/tmp/coordinates.txt" in

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

let box_centeri  i = 
  let pts = Graph.getinfoi_ (g()) i in
  match pts with 
    | x1::y1::x2::y2::x3::y3::x4::y4::[] -> 
	(((x1, y1) +++ (x3, y3)) /// 2 )
   | _ -> raise (Misc.Impossible_Case "Read_coords.box_center")



(*
Check connectedness
  Graph.itern_ (fun src -> 
  Graph.itern_ (fun dst -> 
    Printf.printf "doing %s-%s\n" src dst;
    ignore (Graph.route_dij_ g src dst);
  ) g
) g
*)
