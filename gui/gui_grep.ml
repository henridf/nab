(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc
open GMain

let t = ref (Common.get_time())

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
let show_route = ref true
let show_connectivity = ref false
let show_tree = ref true

let src() = (Param.get Params.nodes) - 1
let dst = 0
 
let route_portion = ref 1.0


let draw_nodes () = 
  Gui_ops.draw_all_nodes(); 
  Gui_ops.draw_node ~emphasize:true (src());
  Gui_ops.draw_node ~emphasize:true dst
  


let refresh ?(clear=true) ()  = (
  Gui_gtk.draw ~clear ();
  if !show_nodes  then  draw_nodes(); 
  if !show_connectivity  then  Gui_ops.draw_connectivity();

  if !show_route && (!rt <> None) then
    Gui_ops.draw_grep_route 
      (Mwsconv.route_nodeid_to_pix (o2v !rt));
)


let running = ref false
let start_stop () = (
  (* if we are in the middle of choosing a node, should we cancel all state? *)
  
  match !running with
    | true -> 
	Gui_gtk.txt_msg "Nodes are frozen ";
	Gui_ctl.stop();
	running := not !running;
	refresh ();
    | false -> 
	Gui_gtk.txt_msg "Nodes are moving ";
	Gui_ctl.startmws ~mws_tick:1. ~rt_tick_ms:1000 ~display_cb:refresh;
	running := not !running;
)


let get_route () = (

  Mob_ctl.stop_all();

  (Gsched.sched())#run(); 
  let routeref = (ref (Route.create())) in
  Gui_hooks.route_done := false;
  let in_mhook = Gui_hooks.grep_route_pktin_mhook routeref in
  let out_mhook = Gui_hooks.grep_route_pktout_mhook routeref in
  Nodes.iter (fun n -> n#clear_pkt_mhooks);
  Nodes.iter (fun n -> n#add_pktin_mhook in_mhook);
  Nodes.iter (fun n -> n#add_pktout_mhook out_mhook);
  (Nodes.node (src()))#originate_app_pkt ~dst;

  (Gsched.sched())#run_until 
  ~continue:(fun () -> 
    Gui_hooks.route_done = ref false;
  );
  
(*
  Gui_ops.draw_ease_route 
    ~lines:true
    ~anchors:false
    ~disks:false
    ~portion:1000.
    (Mwsconv.nodeid_2_pix_route !routeref);
*)

  rt := Some !routeref;
  refresh();
  Mob_ctl.start_all();
)

let choose_node () = (
  (* if nodes are moving around, stop'em *)
  if !running then (
    Gui_ctl.stop();
    running := not !running;
  );
  get_route();
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

let create_buttons_grep() = (

  let ss_tab = create_buttons_common() in

  let checkbox_tab = (GPack.table ~rows:1 ~columns:4 ~homogeneous:false 
    ~row_spacings:0 ~col_spacings:0 ~border_width:0
    ()) in

  ss_tab#attach checkbox_tab#coerce ~left:2 ~top:0 ~right:3 ~bottom:1
    ~xpadding:0 ~ypadding:0  ~expand:`BOTH;
(*  let box2 = GPack.vbox ~spacing: 0 ~border_width: 10
    ~packing: box1#pack () in*)
  

  let checkboxlist = [
    ("Nodes", show_nodes, 0, 0);
    ("Connectivity", show_connectivity, 1, 0);
    ("Route", show_route, 2, 0);
  ] in
  
  List.iter (fun (txt, boolref, left, top) ->
    let btn = (GButton.check_button ~label:txt
      ()) in
    checkbox_tab#attach btn#coerce ~left ~top ~right:(left + 1) 
      ~bottom:(top +  1)  ~xpadding:0 ~ypadding:0  ~expand:`BOTH;
    
    ignore (btn#connect#released 
      ~callback:(fun _ -> 
	boolref := not !boolref;
	refresh () ;
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
      if (!rt <> None) then refresh ();
    ));
  Gui_gtk.set_expose_event_cb (fun _ -> refresh(); false)
  )




    
(* to kill: window#destroy ()*)

