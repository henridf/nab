(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)


open Graph
open Coord
open Route


let draw_node ?(emphasize=false) nid = 
  let cols = [| 
    `RGB ( 40, 60, 60);
    `RGB ( 30, 40, 40);
    `RGB ( 50, 80, 80);|] in

  let (col, target) = if emphasize  then 
    (`NAME "black", true)
  else 
    (cols.(Random.int 3), false)
  in 
  let pos = Gui_conv.pos_mtr_to_pix ((World.w())#nodepos nid)
  in
  Gui_gtk.draw_node ~col ~target pos


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

)

let connect_nodes ?(col=(`NAME "dim grey")) nidlist = (
  let poslist =
  (List.map
    (fun (n1, n2) -> 
      (Gui_conv.pos_mtr_to_pix ((World.w())#nodepos n1)), 
      (Gui_conv.pos_mtr_to_pix ((World.w())#nodepos n2)))
  ) nidlist
  in
  Gui_gtk.draw_segments ~col poslist
)

let draw_connectivity () = (

  let neighborlist = 
  Nodes.fold (fun node l ->
    let nid = node#id in
    let ngbrs =  (World.w())#neighbors nid
    in 
    l@ (List.map (fun ngbr -> (nid, ngbr)) ngbrs))
  []
  in
  connect_nodes neighborlist
)

let draw_all_boxes() = (
  let f point = Gui_conv.pos_mtr_to_pix (Coord.i2f point) in
  let list2segs l = 
    match l with 
      | x1::y1::x2::y2::x3::y3::x4::y4::[] -> [
	  f (x1, y1), f (x2, y2); 
	  f (x1, y1), f (x4, y4);
	  f (x3, y3), f (x2, y2);
	  f (x3, y3), f (x4, y4)
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
	    Gui_conv.pos_mtr_to_pix (Read_coords.box_centeri n),
	    Gui_conv.pos_mtr_to_pix (Read_coords.box_centeri ngbr)];
      ) ngbrs
    ) g
)

let draw_tree ?(col=(`NAME "light grey")) tree =  
  let connect ~parent ~child = 
    Gui_gtk.draw_segments ~col [parent, child]
  in
  NaryTree.iter2 connect tree

let draw_grep_route r = 

  let colors = [| "OrangeRed"; "PaleGreen"; "LightSlateBlue"|] in
  let colindex = ref (-1) in

  let nhops = List.length r
  and nsearches = List.length (List.filter (fun h -> Opt.is_some h.info) r)
  in
  List.iter (fun h -> 
  Log.log#log_notice 
    (lazy (Printf.sprintf "\n\tHop: %s "  (Coord.sprint h.hop) ))) r;

  Log.log#log_notice 
    (lazy (Printf.sprintf "%d Hops, %d Searches"  nhops nsearches ));

  let rec draw_hops_ = function
    | [] -> ()
    | hop1::hop2::r -> 
	Gui_gtk.draw_segments ~thick:2 [(hop1.hop, hop2.hop)];
	let n = hop1.hop in
	Gui_gtk.draw_node n;
	draw_hops_ (hop2::r); 
    | hop1::r ->   Gui_gtk.draw_node ~target:true hop1.hop 
  in

  let draw_disks_ route = (
    let rec aux route = (
      match route with 
	| [] -> ()
	| hop1::hop2::r -> (
	    begin
	      match hop1.info with
		| None -> ()
		| Some flood -> 
		    let radius = 
		      (Misc.f2i (sqrt (Misc.i2f (Coord.disti_sq hop1.hop hop2.hop)))) 
		    in
		    Printf.printf "drawing circle, radius %d" radius;
		    flush stdout;
		    Gui_gtk.draw_circle ~centr:hop1.hop ~radius;
	    end;
	    aux (hop2::r);
	  )
	| hop1::r -> ()
    ) in
    let route_only_anchors = 
      List.filter (fun hop -> hop.info != None) route
    in aux (route_only_anchors@[Misc.listlast route])
  )
  in 
  let rec draw_trees_ = function 
    | hop1::hop2::r -> (
	begin
	  match hop1.info with
	    | None -> ()
	    | Some flood -> 
		colindex := (!colindex + 1) mod (Array.length colors);
		let pixtree = NaryTree.map flood 
		  ~f:(fun nid -> Gui_conv.pos_mtr_to_pix
		    ((World.w())#nodepos nid))
		in
		Log.log#log_notice 
		  (lazy (Printf.sprintf "Tree touched %d nodes"
		    (NaryTree.size flood)));
		draw_tree ~col:(`NAME colors.(!colindex)) pixtree;
		draw_node ~emphasize:true (NaryTree.root flood)
	  end;		
	  draw_trees_ (hop2::r);
	)
      | _ -> ()
  in
  draw_disks_ r;
  draw_trees_ r; 
  draw_hops_ r
    
let draw_ease_route 
    ?(lines=true) 
    ?(anchors=true) 
    ?(disks=true) 
    ?(portion=1.0)
    r  = (
      let colors = [|
	"blue";
	"dim grey";
	"green";
	"olive drab";
	"coral";
	"tomato"|] in

      let portion_ = 
	if (portion < 0.0 || portion > 1.0) then 1.0 else portion in 
	
      let hops_not_drawn = (List.length r) -
	truncate ((float (List.length r)) *. portion_)
      in

      let rec draw_disks_ route = (
	match route with 
	  | [] -> ()
	  | hop1::hop2::r when (List.length r <= hops_not_drawn)
	      -> (
		Gui_gtk.txt_msg 
		(Printf.sprintf "Anchor Age: %d seconds" 
		  (truncate (Opt.get hop2.info).anchor_age))
	      )
	  | hop1::hop2::r -> (

	      (* we assume that the rectangle ratio of the window and the world
		 are the same, otherwise this would not be a circle *)
	      Gui_gtk.draw_circle ~centr:hop1.hop 
	      ~radius:(Gui_conv.x_mtr_to_pix (Opt.get hop1.info).searchcost);
	      draw_disks_ (hop2::r);
	    )
	  | hop1::r -> ()
      ) in

      let colindex = ref (-1) in
      let rec draw_anchors_  firsthop route = (

	let draw_anchor_line hop = 
	  colindex := (!colindex + 1) mod (Array.length colors);
	  if lines then (
	    Gui_gtk.draw_segments ~col:(`NAME colors.(!colindex))
	    [hop.hop, (Opt.get hop.info).anchor];
	  );
	  if (anchors) then (
	    Gui_gtk.draw_cross ~diag:false ~col:(`NAME colors.(!colindex)) ~target:true
	    (Opt.get hop.info).anchor
	  )
	in

	match route with 
	  | [] -> ()
	  | hop1::hop2::r when (List.length r <= hops_not_drawn) -> ()
	  | hop1::hop2::r when firsthop -> (
	      draw_anchor_line hop1;
	      draw_anchors_ false (hop1::hop2::r)
	    )
	  | hop1::hop2::r -> (
	      if (Opt.get hop2.info).anchor_age <> 
		(Opt.get hop1.info).anchor_age then draw_anchor_line hop2;
	      draw_anchors_ false (hop2::r);
	    )
	  | hop1::r -> ()
      ) in

      let rec draw_route_ route = (
	match route with 
	  | [] -> ()
	  | hop1::hop2::r when (List.length r <= hops_not_drawn)
	      -> (
		if List.length r = 0 then (
		  Gui_gtk.draw_node ~target:true ~col:(`NAME "red")
		  hop2.hop;
		  Gui_gtk.draw_segments [(hop1.hop, hop2.hop)];
		  let n = hop1.hop in
		  let c1 = Coord.xx n  
		  and c2 = Coord.yy n  in
		  Gui_gtk.draw_node n;
		  (*	      let segments = [(c1 - 3, c2), (c1 + 3, c2) ; (c1, c2 - 3), (c1, c2 + 3)] in
			      Gui_gtk.draw_segments segments ;*)
		  draw_route_ (hop2::r);
		  
	      )
	      )

	  | hop1::hop2::r -> (
	      (*	  Graphics.set_color (color ~hop:!i ~routelength:len);
			  Graphics.set_color (Graphics.rgb 0 0 0);*)
	      Gui_gtk.draw_segments [(hop1.hop, hop2.hop)];
	      let n = hop1.hop in
	      let c1 = Coord.xx n  
	      and c2 = Coord.yy n  in
	      Gui_gtk.draw_node n;
(*	      let segments = [(c1 - 3, c2), (c1 + 3, c2) ; (c1, c2 - 3), (c1, c2 + 3)] in
	      Gui_gtk.draw_segments segments ;*)
	      draw_route_ (hop2::r);
	    )
	  | hop1::r -> (
	      Gui_gtk.draw_node ~target:true hop1.hop;
	    )
      )
      in
      draw_route_ r;
      if disks then (draw_disks_ r);
      draw_anchors_ true r;
    )


let get_node_cb_sig_id = ref None

let remove_get_node_cb() = 
  let id = Opt.get !get_node_cb_sig_id in
  Gui_gtk.remove_button_press_cb id

let user_pick_node ?(msg = "Pick a node!") ~node_picked_cb () = (
  Gui_gtk.txt_msg msg;
  get_node_cb_sig_id := Some (
    Gui_gtk.install_button_press_cb 
    (fun b -> 

      let x, y = (Misc.f2i (GdkEvent.Button.x b), Misc.f2i (GdkEvent.Button.y b)) in
      let node = Gui_conv.closest_node_at (x, y)
      in
      remove_get_node_cb();
      node_picked_cb node;

      (* returning true or false from this callback does not seem to make any
	 difference. Read somewhere (API or tut) that this is because it will
	 then call the default handler and use the return value of that one. 
	 apparently we would have to use the *connect_after (or stg like that)
	 call to be called after the default, and then our return value would
	 be taken into account *)
      true)
  )
)
  
let validate_node_input s = 
  try 
    let n = Misc.s2i s in
    if n < 0 || n > (Param.get Params.nodes) - 1 then 
      (false, 0)
    else 
      (true, n)
 with _ -> (false, 0)

let dialog_pick_node ?default ~node_picked_cb () = 
  let n = ref (-1) in
  let dialog =
      GWindow.window ~kind:`DIALOG ~border_width:10 ~title:"Node ID" () in
    let dvbx = GPack.box `VERTICAL ~packing:dialog#add () in
    let entry  = GEdit.entry ~max_length:5 ~packing: dvbx#add () in
    if Opt.is_some default then entry#set_text (Misc.i2s (Opt.get default));
    let dquit = GButton.button ~label:"OK" ~packing: dvbx#add () in 
    ignore (dquit#connect#clicked ~callback:
      begin fun _ ->
	let ok, node = validate_node_input entry#text in
	if ok then begin
	  flush stdout;
          dialog#destroy ();
	  node_picked_cb node
	end
	else 
	  entry#set_text "";
      end);
    dialog#show ()
    
      




  
