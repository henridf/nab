(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Header *)







(* Notes on hacks/changes for quick cens dirty job.
   create_buttons_trees was just a cutnpaste/change of create_buttons_ease - 
   no attempt at figuring out how things could be less hardcoded and more
   fnexible. same for set_src_trees w.r.t set_src.
   (basically we shoul distinguish btw generic stuff like offering a
   fun to pick a node, etc, and stuff which is app-specific (like drawing a
   route (set_src below), etc).

   in install_get_node_cb() , simply replaced the call to set_src with
   set_tree_src. 

*)

   


open Misc
open GMain

let run_id = ref None
let t = ref (Time.get_time())

let rt = ref None (* keep a copy of last route around so expose_event can
		     redraw it *)

let start_stop_btn = ref None
let start_stop_tab:GPack.table option ref = ref None
let ss_btn() = o2v !start_stop_btn
let ss_tab() = o2v !start_stop_tab
let choose_route_btn = ref None
let rt_btn() = o2v !choose_route_btn

let show_nodes = ref true
let show_route_lines = ref true
let show_route_anchors = ref true
let show_route_disks = ref true
let show_connectivity = ref false
let show_tree = ref true


let route_portion = ref 1.0

let run() = (

  Gui_gtk.set_expose_event_cb (fun _ -> true);
  
  t := (Time.get_time());
  let continue() = ((Time.get_time()) < !t +. 1.0) in
  (Sched.s())#run_until~continue;

  if !show_nodes  then  Gui_ops.draw_all_nodes(); 
  Gui_gtk.draw ~clear:true ();
  true
)
  


let refresh ?(clear=false) ()  = (
  if !show_nodes  then  Gui_ops.draw_all_nodes(); 
  if !show_connectivity  then  Gui_ops.draw_connectivity(); 
  (*
    Gui_ops.draw_all_routes 
    (); 
    Gui_ops.draw_all_boxes(); 
  *)
  Gui_gtk.draw ~clear ();
  if (!rt <> None) then
    Gui_ops.draw_route 
      ~lines:!show_route_lines
      ~anchors:!show_route_anchors
      ~disks:!show_route_disks
      ~portion:!route_portion
      (Mwsconv.mtr_2_pix_route (o2v !rt));
  false
)

let refresh_cb _ = refresh ()

let stop() = (
  Gui_gtk.txt_msg "Nodes are frozen ";

  (* calling function is responsible for ensuring that !run_id <> None , ie
     that we are indeed running *)
  Mob_ctl.stop_all();
  
  (* this call is to "purge" all mobility events that might be still in the
     scheduler. normally this should be done by the mob itself when we stop it,
     but this is pending the ability to cancel events in the scheduler (see
     general_todo.txt) *)

  (Sched.s())#run(); 

  Timeout.remove (o2v !run_id);
  run_id := None;
  Gui_gtk.set_expose_event_cb refresh_cb
)

let start() = (

  rt := None;
  Gui_gtk.txt_msg "Nodes are moving ";
  Mob_ctl.start_all();
  ignore(run());
  run_id := Some (Timeout.add ~ms:300 ~callback:run);
)

let start_stop () = (
  (* if we are in the middle of choosing a node, should we cancel all state? *)
  match !run_id with
    | Some id -> stop()
    | None -> start()
)



let get_node_cb_sig_id = ref None

let remove_get_node_cb() = 
  let id = o2v !get_node_cb_sig_id in
  Gui_gtk.remove_button_press_cb id


let set_src x y = (
  remove_get_node_cb();

  Printf.printf "src is at %d %d\n" x y; flush stdout;
  let srcnode = Mwsconv.closest_node_at (x, y)
  in

  let routeref = (ref (Route.create())) in
  Gui_hooks.route_done := false;
  let in_mhook = Gui_hooks.ease_route_pktin_mhook routeref in
  let out_mhook = Gui_hooks.ease_route_pktout_mhook routeref in
  Nodes.gpsiter (fun n -> n#clear_pkt_mhooks);
  Nodes.gpsiter (fun n -> n#add_pktin_mhook in_mhook);
  Nodes.gpsiter (fun n -> n#add_pktout_mhook out_mhook);
  (Nodes.node srcnode)#originate_app_pkt ~dst:0;

  (Sched.s())#run_until 
  ~continue:(fun () -> 
    Gui_hooks.route_done = ref false;
  );
  
  Printf.printf "Route:\n %s\n" (Route.sprint ( !routeref));flush stdout;
  ignore (Route.route_valid !routeref 
    ~dst:((World.w())#nodepos 0) 
    ~src:((World.w())#nodepos srcnode));
  Gui_ops.draw_route 
    ~lines:!show_route_lines
    ~anchors:!show_route_anchors
    ~disks:!show_route_disks
    ~portion:!route_portion
    (Mwsconv.mtr_2_pix_route !routeref);
  
  rt := Some !routeref;
)

let get_tree_sink sink = (
  let sinkdiffagent = !Diff_agent.agents_array.(sink) in
  let sink_seqno = (sinkdiffagent#seqno()) - 1 in
    Array.to_list (
      Nodes.mapi (fun nid -> 
	  let diffagent = !Diff_agent.agents_array.(nid) in
	  if (diffagent#is_closest_sink ~op:(>=) sink) then (
	    let rt = diffagent#get_rtab in 
	    let nexthop = Rtab.nexthop ~rt ~dst:sink in
	    
	    match nexthop with 
	      | _ when (nid = sink) -> (0, 0)
	      | None -> (0, 0)
	      | Some nh  -> (nh, nid)

(*
  let rt = diffagent#get_rtab in 
  let nexthop = Rtab.nexthop ~rt ~dst:sink in
  let seqno = Rtab.seqno ~rt ~dst:sink in

	    
	  match (nexthop, seqno) with 
	    | _ when (nid = sink) -> (0, 0)
	    | None, None -> (0, 0)
	    | (Some nh, Some sn) when (sn < sink_seqno) -> (0, 0)
	    | (Some nh, Some sn) when (sn > sink_seqno) ->  raise 
		(Failure 
		  "Gui_ctl.set_tree: node had higher seqno for dst than dst itself")
	    | (Some nh, Some sn) when (sn = sink_seqno) -> (nh, nid)
	    | _ -> raise (Misc.Impossible_Case "Gui_ctl.set_tree")
*)
	  ) else 0,0


      )
    )  
)

let make_tree_sink sink = 
(Nodes.node(sink))#originate_app_pkt  ~dst:123



let set_tree_src x y = (
  remove_get_node_cb();

  Printf.printf "src is at %d %d\n" x y; flush stdout;
  let sink2 = Mwsconv.closest_node_at (x, y) in
(*  let sink2 = Random.int (Param.get Params.nodes) in
  let sink3 = Random.int (Param.get Params.nodes) in
  let sink4 = Random.int (Param.get Params.nodes) in
  let sink5 = Random.int (Param.get Params.nodes) in*)

(*  Diff_agent.sinks := [0;sink2;sink3;sink4;sink5 ];

  Diff_agent.sinks := [0;sink2 ];
  make_tree_sink 0;
  make_tree_sink sink2;
  make_tree_sink sink3;
  make_tree_sink sink4;
  make_tree_sink sink5;*)
  (Sched.s())#run(); 

  let tree1 = get_tree_sink 0 
  and tree2 = get_tree_sink sink2 
in

  
  Gui_ops.connect_nodes ~col:(`NAME "black") tree1;
  Gui_ops.connect_nodes ~col:(`NAME "red") tree2;
(*  Gui_ops.connect_nodes ~col:(`NAME "green") tree3;
  Gui_ops.connect_nodes ~col:(`NAME "blue") tree4;
  Gui_ops.connect_nodes ~col:(`NAME "white") tree5;*)

  Gui_ops.draw_node ~emphasize:true 0;
  Gui_ops.draw_node ~emphasize:true sink2;
(*  Gui_ops.draw_node ~emphasize:true sink3;
  Gui_ops.draw_node ~emphasize:true sink4;
  Gui_ops.draw_node ~emphasize:true sink5;*)


  
)

(*
let set_tree_src x y = (
  remove_get_node_cb();

  Printf.printf "src is at %d %d\n" x y; flush stdout;
  let sinksarr = Array.init 40 
    (fun _ -> Random.int (Param.get Params.nodes)) in
 
  Diff_agent.sinks := Array.to_list sinksarr;
  Array.iter (fun i -> make_tree_sink i) sinksarr;

  (Sched.s())#run(); 

  let treesarr = Array.map (fun i -> get_tree_sink i) sinksarr in
  let colors = [|
    "blue";
    "dim grey";
    "green";
    "purple"; 
    "yellow";
    "pink";
    "olive drab";
    "coral";
    "tomato"; "black"; "white"; "red"|] in
  
  Array.iteri (fun i tree -> 
    let colindex = (i mod (Array.length colors)) in
    let col = colors.(colindex) in
    Gui_ops.connect_nodes ~col:(`NAME col) tree;
  ) treesarr
  
)*)

let install_get_node_cb() = (
  Gui_gtk.txt_msg "Choisissez la source";
  get_node_cb_sig_id := Some (
    Gui_gtk.install_button_press_cb 
    (fun b -> 
      let x, y = (GdkEvent.Button.x b, GdkEvent.Button.y b) in
      begin
	Gui_gtk.txt_msg "Calcul de la route..";	    
	(* set_src (f2i x) (f2i y);*)
	 set_tree_src (f2i x) (f2i y);
      end;
      (* returning true or false from this callback does not seem to make any
	 difference. Read somewhere (API or tut) that this is because it will
	 then call the default handler and use the return value of that one. 
	 apparently we would have to use the *connect_after (or stg like that)
	 call to be called after the default, and then our return value would
	 be taken into account *)
      true)
  )
)

let choose_node () = (
  (* if nodes are moving around, stop'em *)
  begin 
    match !run_id with 
      | Some id -> stop()
      | _ -> ()
  end;
  install_get_node_cb();
)

  
let create_buttons_common() = (

  let ss_tab = (GPack.table ~rows:1 ~columns:3 ~homogeneous:false 
    ~row_spacings:0 ~col_spacings:0 ~border_width:0
    ~packing:(Gui_gtk.packer()) ()) in

  start_stop_btn := Some (GButton.toggle_button ~draw_indicator:false
    ~label:"start/stop" ());
  ignore ((ss_btn())#connect#released ~callback:(start_stop));
  ss_tab#attach (ss_btn())#coerce ~left:0 ~top:0 ~right:1 ~bottom:1
    ~xpadding:0 ~ypadding:0  ~expand:`BOTH;

  choose_route_btn := Some (GButton.toggle_button ~draw_indicator:false
    ~label:"draw a route" ()) ;
  ignore ((rt_btn())#connect#released ~callback:(choose_node));
  ss_tab#attach (rt_btn())#coerce ~left:1 ~top:0 ~right:2 ~bottom:1
    ~xpadding:0 ~ypadding:0  ~expand:`BOTH;

  ss_tab
)

let create_buttons_ease() = (

  let ss_tab = create_buttons_common() in

  let checkbox_tab = (GPack.table ~rows:1 ~columns:4 ~homogeneous:false 
    ~row_spacings:0 ~col_spacings:0 ~border_width:0
    ()) in

  ss_tab#attach checkbox_tab#coerce ~left:2 ~top:0 ~right:3 ~bottom:1
    ~xpadding:0 ~ypadding:0  ~expand:`BOTH;
(*  let box2 = GPack.vbox ~spacing: 0 ~border_width: 10
    ~packing: box1#pack () in*)
  

  let checkboxlist = [
    ("Hide nodes", show_nodes, 0, 0);
    ("Hide Anchors", show_route_anchors, 1, 0);
    ("Hide Directions", show_route_lines, 2, 0);
    ("Hide Disks", show_route_disks, 3, 0);
  ] in
  
  List.iter (fun (txt, boolref, left, top) ->
    let btn = (GButton.check_button ~label:txt
      ()) in
    checkbox_tab#attach btn#coerce ~left ~top ~right:(left + 1) 
      ~bottom:(top +  1)  ~xpadding:0 ~ypadding:0  ~expand:`BOTH;
    
    ignore (btn#connect#released 
      ~callback:(fun _ -> 
	boolref := not !boolref;
	ignore (refresh ~clear:true ()) ;
      )
    )) checkboxlist;

  let adj =
    GData.adjustment ~lower:0. ~upper:1001. ~step_incr:1. ~page_incr:100. () in
  let sc = GRange.scale `HORIZONTAL ~adjustment:adj ~draw_value:false
    ~packing:(Gui_gtk.packer()) () in
    
  ignore (adj#connect#value_changed
    ~callback:(fun () -> 
      Printf.printf "value %f\n" adj#value; flush stdout;
      
      route_portion := 
      if       adj#value > 990.0 then 1.0 else
      adj#value/.1000.;
      if (!rt <> None) then ignore (refresh() ~clear:true);
    ));



(*  ignore (counter#connect#changed ~callback:(fun n -> 
    Gui_gtk.txt_msg (Printf.sprintf "New value %s.." (string_of_int n))));
*)
  )


let create_buttons_trees() = (

  let ss_tab = create_buttons_common() in

  let checkbox_tab = (GPack.table ~rows:1 ~columns:4 ~homogeneous:false 
    ~row_spacings:0 ~col_spacings:0 ~border_width:0
    ()) in

  ss_tab#attach checkbox_tab#coerce ~left:2 ~top:0 ~right:3 ~bottom:1
    ~xpadding:0 ~ypadding:0  ~expand:`BOTH;
  

  let checkboxlist = [
    ("Hide nodes", show_nodes, 0, 0);
    ("Hide connectivity", show_connectivity, 1, 0);
    ("Hide Tree", show_tree, 2, 0);
  ] in
  
  List.iter (fun (txt, boolref, left, top) ->
    let btn = (GButton.check_button ~label:txt
      ()) in
    checkbox_tab#attach btn#coerce ~left ~top ~right:(left + 1) 
      ~bottom:(top +  1)  ~xpadding:0 ~ypadding:0  ~expand:`BOTH;
    
    ignore (btn#connect#released 
      ~callback:(fun _ -> 
	boolref := not !boolref;
	ignore (refresh ~clear:true ()) ;
      )
    )) checkboxlist;

  )

    
(* to kill: window#destroy ()*)

