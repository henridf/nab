(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Graph
open Coord

let draw_node_time nid t = 
  let hilite = (nid < Param.get Params.ntargets) in 
  let (_, pos) = Gui_pos.node_pos_inst nid (Common.get_time())
  in
  Gui_gtk.draw_node hilite pos
  
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
  let  list2segs l = 
    match l with 
      | x1::y1::x2::y2::x3::y3::x4::y4::[] -> [
	  (x1, y1), (x2, y2); 
	  (x1, y1), (x4, y4);
	  (x3, y3), (x2, y2);
	  (x3, y3), (x4, y4)
	]
      | _ -> raise (Misc.Impossible_Case "Gui_gtk.draw_all_boxes")
  in
  let g = Read_coords.g() in
  Graph.itern_ 
    (fun n -> 
(*      let x1::y1::_ = Graph.getinfo_ g n in*)
      Gui_gtk.draw_segments (list2segs (Graph.getinfo_ g n));
    ) g

)

let draw_route r = (
  let rec draw_disks_ route = (

    match route with 
      | [] -> ()
      | hop1::hop2::r -> (
	  (* we assume that the rectangle ratio of the window and the world
	     are the same, otherwise this would not be  circle *)
	  Gui_gtk.draw_circle ~centr:hop1.Route.hop ~radius:(Gui_hooks.x_mtr_to_pix hop1.Route.searchcost);
	  draw_disks_ (hop2::r);
	)
      | hop1::r -> ()
  ) in

  let rec draw_route_ route = (
    match route with 
      | [] -> ()
      | hop1::hop2::r -> (
(*	  Graphics.set_color (color ~hop:!i ~routelength:len);
	  Graphics.set_color (Graphics.rgb 0 0 0);*)
	  Gui_gtk.draw_segments [(hop1.Route.hop, hop2.Route.hop)];
	  let n = hop1.Route.hop in
	  let c1 = Coord.xx n  
	  and c2 = Coord.yy n  in
	  let segments = [(c1 - 3, c2), (c1 + 3, c2) ; (c1, c2 - 3), (c1, c2 + 3)] in
	  Gui_gtk.draw_segments segments ;
	  draw_route_ (hop2::r);
	)
      | hop1::r -> ()
  )
  in
  draw_route_ r;
  draw_disks_ r;
)
