(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Graph
open Coord
open Misc

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
  let pos = Mwsconv.pos_mtr_to_pix ((Gworld.world())#nodepos nid)
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
      (Mwsconv.pos_mtr_to_pix ((Gworld.world())#nodepos n1)), 
      (Mwsconv.pos_mtr_to_pix ((Gworld.world())#nodepos n2)))
  ) nidlist
  in
  Gui_gtk.draw_segments ~col poslist
)

let draw_connectivity () = (

  let neighborlist = 
  Nodes.fold (fun node l ->
    let nid = node#id in
    let ngbrs =  (Gworld.world())#neighbors nid
    in 
    l@ (List.map (fun ngbr -> (nid, ngbr)) ngbrs))
  []
  in
  connect_nodes neighborlist
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
      Printf.printf "hops_not_drawn %d\n" hops_not_drawn; 
      Printf.printf "portion: %f\n" portion_;flush stdout;

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
	      Gui_gtk.draw_circle ~centr:hop1.Route.hop ~radius:(Mwsconv.x_mtr_to_pix hop1.Route.searchcost);
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
		if lines then (
		  Gui_gtk.draw_segments ~col:(`NAME colors.(!colindex))
		    [hop1.Route.hop, hop1.Route.anchor];
		);
		if (anchors) then (
		  Gui_gtk.draw_cross ~diag:false ~col:(`NAME colors.(!colindex)) ~target:true
		    hop1.Route.anchor
		)
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
	      -> (
		if List.length r = 0 then (
		  Gui_gtk.draw_node ~target:true ~col:(`NAME "red")
		  hop2.Route.hop;
		  Gui_gtk.draw_segments [(hop1.Route.hop, hop2.Route.hop)];
		  let n = hop1.Route.hop in
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
	      Gui_gtk.draw_segments [(hop1.Route.hop, hop2.Route.hop)];
	      let n = hop1.Route.hop in
	      let c1 = Coord.xx n  
	      and c2 = Coord.yy n  in
	      Gui_gtk.draw_node n;
(*	      let segments = [(c1 - 3, c2), (c1 + 3, c2) ; (c1, c2 - 3), (c1, c2 + 3)] in
	      Gui_gtk.draw_segments segments ;*)
	      draw_route_ (hop2::r);
	    )
	  | hop1::r -> (
	      Gui_gtk.draw_node ~target:true hop1.Route.hop;
	    )
      )
      in
      draw_route_ r;
      if disks then (draw_disks_ r);
      draw_anchors_ true r;
    )


let get_node_cb_sig_id = ref None

let remove_get_node_cb() = 
  let id = o2v !get_node_cb_sig_id in
  Gui_gtk.remove_button_press_cb id

let user_pick_node ?(msg = "Pick a node!") ~node_picked_cb () = (
  Gui_gtk.txt_msg msg;
  get_node_cb_sig_id := Some (
    Gui_gtk.install_button_press_cb 
    (fun b -> 

      let x, y = (f2i (GdkEvent.Button.x b), f2i (GdkEvent.Button.y b)) in
      let node = Mwsconv.closest_node_at (x, y)
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


  
