(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Graph
open Coord

let draw_node_time nid t = 
  let cols = [| 
    `RGB ( 40, 60, 60);
    `RGB ( 30, 40, 40);
    `RGB ( 50, 80, 80);|] in

  let (col, target) = if (nid < Param.get Params.ntargets) then 
    (`NAME "red", true)
  else 
    (cols.(Random.int 3), false)
  in 
  let (_, pos) = Gui_pos.node_pos_inst nid (Common.get_time())
  in

  Gui_gtk.draw_node ~col ~target pos
  
let draw_nodes_time nidlist t = 
  List.iter (fun nid -> draw_node_time nid t) nidlist

let draw_node nid = 

  draw_node_time nid (Common.get_time())

let draw_nodes nidlist = 
  List.iter (fun nid -> draw_node nid) nidlist


let draw_all_nodes() = (
  let rec rec_ n = 
    match n with 
      |	0  -> ()
      | nid -> (
	  draw_node (nid - 1);   
	  rec_ (nid - 1)
	)
  in
  rec_ (Param.get Params.nodes);
  rec_ ((Param.get Params.ntargets) + 1);

)



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
      Gui_gtk.draw_segments (list2segs (Graph.getinfo_ g n));
    ) g

)


let draw_all_routes() = (
  let g = Read_coords.g() in
  Graph.iteri_ 
    (fun n -> 
      let ngbrs = (Graph.neigborsi_ g n) in
      List.iter (
	fun ngbr ->
	  Gui_gtk.draw_segments [
	    (Read_coords.box_centeri n),
	    (Read_coords.box_centeri ngbr)];
      ) ngbrs
    ) g
)

let draw_route 
    ?(lines=true) 
    ?(anchors=true) 
    ?(disks=true) 
    ~portion
    r  = (
      let colors = [|
	"blue";
	"dim grey";
	"green";
	"olive drab";
	"coral";
	"tomato"|] in

      let hops_not_drawn = (List.length r) -
	truncate ((float (List.length r)) *. portion)
      in
      Printf.printf "hops_not_drawn %d\n" hops_not_drawn; flush stdout;
      let rec draw_disks_ route = (
	match route with 
	  | [] -> ()
	  | hop1::hop2::r when (List.length r <= hops_not_drawn)
	      -> (
		Gui_gtk.txt_msg (Printf.sprintf "Age de l'ancre courante: %d secondes" (truncate hop2.Route.anchor_age))
	      )
	  | hop1::hop2::r -> (

	      (* we assume that the rectangle ratio of the window and the world
		 are the same, otherwise this would not be a circle *)
	      Gui_gtk.draw_circle ~centr:hop1.Route.hop ~radius:(Gui_hooks.x_mtr_to_pix hop1.Route.searchcost);
	      draw_disks_ (hop2::r);
	    )
	  | hop1::r -> ()
      ) in

      let colindex = ref (-1) in
      let rec draw_anchors_  firsthop route = (
	match route with 
	  | [] -> ()
	  | hop1::hop2::r when (List.length r <= hops_not_drawn)
	      -> ()
	  | hop1::hop2::r -> (
	      if (firsthop || ((hop2.Route.anchor_age) <> (hop1.Route.anchor_age))) then (
		(*	    Printf.printf "drawing anchor from %s to %s\n" 
			    (Coord.sprint hop1.Route.hop) (Coord.sprint hop1.Route.anchor);*)
		colindex := (!colindex + 1) mod (Array.length colors);
		if lines then
		  Gui_gtk.draw_segments ~col:(`NAME colors.(!colindex))
		    [hop1.Route.hop, hop1.Route.anchor];
		if anchors then
		  Gui_gtk.draw_cross ~diag:false ~col:(`NAME colors.(!colindex)) ~target:true
		    hop1.Route.anchor
		  
	      );
	      
	      (*	    Printf.printf "Ages are %f and %f\n"  hop1.Route.anchor_age hop2.Route.anchor_age*)

	      draw_anchors_ false (hop2::r)
	    )
	  | hop1::r -> ()
      ) in

      let rec draw_route_ route = (
	match route with 
	  | [] -> ()
	  | hop1::hop2::r when (List.length r <= hops_not_drawn)
	      -> ()
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
      if disks then (draw_disks_ r);
      draw_anchors_ true r;
    )

