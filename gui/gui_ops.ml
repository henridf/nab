(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Graph

let draw_node_time nid t = 
  let (_, pos) = Gui_pos.node_pos_inst nid (Common.get_time())
  in
  Gui_gtk.draw_node pos
  
let draw_nodes_time nidlist t = 
  List.iter (fun nid -> draw_node_time nid t) nidlist

let draw_node nid = 
  draw_node_time nid (Common.get_time())

let draw_nodes nidlist = 
  List.iter (fun nid -> draw_node nid) nidlist


let draw_all_nodes() = 
  let rec rec_ n = 
    match n with 
      |	0  -> ()
      | nid -> (
	  draw_node (nid - 1);   
	  rec_ (nid - 1)
	)
  in
  rec_ (Param.get Params.nodes)



let draw_all_boxes() = (
  let rec list2segs l s = 
    match l with 
      | x1::y1::x2::y2::x3::y3::x4::y4::[] -> [
	  (x1, y1), (x2, y2); 
	  (x1, y1), (x4, y4);
	  (x3, y3), (x2, y2);
	  (x3, y3), (x4, y4)
	]
      | _ -> raise (Misc.Impossible_Case "Gui_gtk.draw_segments")
  in
  
  let g = Read_coords.g() in
  Graph.itern_ 
    (fun n -> 
(*      let x1::y1::_ = Graph.getinfo_ g n in*)
      Gui_gtk.draw_segments (list2segs (Graph.getinfo_ g n) [])
    ) g

)
