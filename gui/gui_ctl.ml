(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc
open GMain

let run_id = ref None
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
let show_route_disks = ref true

let route_portion = ref 1.0

let run() = (
  Gui_gtk.set_expose_event_cb (fun _ -> true);
  
  t := (Common.get_time());
  let continue() = ((Common.get_time()) < !t +. 1.0) in
  (Gsched.sched())#run_until~continue;
  if !show_nodes  then  Gui_ops.draw_all_nodes(); 
  Gui_gtk.draw ~clear:true ();
  true
)
  


let refresh ?(clear=false) ()  = (
  if !show_nodes  then  Gui_ops.draw_all_nodes(); 
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
      (Gui_hooks.mtr_2_pix_route (o2v !rt));
  false
)

let refresh_cb _ = refresh ()

let stop() = (
  Gui_gtk.txt_msg "Nodes are frozen ";

  (* calling function is responsible for ensuring that !run_id <> None , ie
     that we are indeed running *)
  Mob.stop_all();
  
  (* this call is to "purge" all mobility events that might be still in the
     scheduler. normally this should be done by the mob itself when we stop it,
     but this is pending the ability to cancel events in the scheduler (see
     general_todo.txt) *)

  (Gsched.sched())#run(); 

  Timeout.remove (o2v !run_id);
  run_id := None;
  Gui_gtk.set_expose_event_cb refresh_cb
)

let start() = (

  rt := None;
  Gui_gtk.txt_msg "Nodes are moving ";
  Mob.start_all();
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
  let srcnode = Gui_hooks.closest_node_at (x, y)
  in

  let routeref = (ref (Route.create())) in
  Gui_hooks.route_done := false;
  let in_mhook = Gui_hooks.ease_route_pktin_mhook routeref in
  let out_mhook = Gui_hooks.ease_route_pktout_mhook routeref in
  Nodes.iter (fun n -> n#clear_pkt_mhooks);
  Nodes.iter (fun n -> n#add_pktin_mhook in_mhook);
  Nodes.iter (fun n -> n#add_pktout_mhook out_mhook);
  (Nodes.node srcnode)#originate_app_pkt ~dstid:0;

  (Gsched.sched())#run_until 
  ~continue:(fun () -> 
    Gui_hooks.route_done = ref false;
  );
  
  Printf.printf "Route:\n %s\n" (Route.sprint ( !routeref));flush stdout;
  (*  ignore (Route.route_valid !routeref ~dst:(Nodes.node 0)#pos ~src:(Nodes.node
    srcnode)#pos);*)
  Gui_ops.draw_route 
    ~lines:!show_route_lines
    ~anchors:!show_route_anchors
    ~disks:!show_route_disks
    ~portion:!route_portion
    (Gui_hooks.mtr_2_pix_route !routeref);
  
  rt := Some !routeref;
)


let install_get_node_cb() = (
  Gui_gtk.txt_msg "Choisissez la source";
  get_node_cb_sig_id := Some (
    Gui_gtk.install_button_press_cb 
    (fun b -> 
      let x, y = (GdkEvent.Button.x b, GdkEvent.Button.y b) in
      begin
	Gui_gtk.txt_msg "Calcul de la route..";	    
	set_src (f2i x) (f2i y);
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
	
  
let create_buttons() = (

  let ss_tab = (GPack.table ~rows:1 ~columns:3 ~homogeneous:false 
    ~row_spacings:3 ~col_spacings:3 ~border_width:10
    ~packing:(Gui_gtk.packer()) ()) in

  start_stop_btn := Some (GButton.toggle_button ~draw_indicator:false
    ~label:"start/stop" ());
  ignore ((ss_btn())#connect#released ~callback:(start_stop));
  ss_tab#attach (ss_btn())#coerce ~left:0 ~top:0 ~right:1 ~bottom:1
    ~xpadding:0 ~ypadding:0  ~expand:`BOTH;

  choose_route_btn := Some (GButton.toggle_button ~draw_indicator:false
    ~label:"dessiner une route" ()) ;
  ignore ((rt_btn())#connect#released ~callback:(choose_node));
  ss_tab#attach (rt_btn())#coerce ~left:1 ~top:0 ~right:2 ~bottom:1
    ~xpadding:0 ~ypadding:0  ~expand:`BOTH;

  let checkbox_tab = (GPack.table ~rows:2 ~columns:2 ~homogeneous:false 
    ~row_spacings:3 ~col_spacings:3 ~border_width:10
    ()) in

  ss_tab#attach checkbox_tab#coerce ~left:2 ~top:0 ~right:3 ~bottom:1
    ~xpadding:0 ~ypadding:0  ~expand:`BOTH;
(*  let box2 = GPack.vbox ~spacing: 0 ~border_width: 10
    ~packing: box1#pack () in*)
  

  let checkboxlist = [
    ("Cacher noeuds", show_nodes, 0, 0);
    ("Cacher ancres", show_route_anchors, 0, 1);
    ("Cacher directions", show_route_lines, 1, 0);
    ("Cacher disques", show_route_disks, 1, 1);
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
    GData.adjustment ~lower:0. ~upper:100. ~step_incr:1. ~page_incr:10. () in
  let sc = GRange.scale `HORIZONTAL ~adjustment:adj ~draw_value:false
    ~packing:(Gui_gtk.packer()) () in
    
  ignore (adj#connect#value_changed
    ~callback:(fun () -> 
      route_portion := adj#value/.100.;
      if (!rt <> None) then ignore (refresh() ~clear:true);
    ));



(*  ignore (counter#connect#changed ~callback:(fun n -> 
    Gui_gtk.txt_msg (Printf.sprintf "New value %s.." (string_of_int n))));
*)
  )    
(* to kill: window#destroy ()*)

