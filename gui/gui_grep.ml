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







open Misc
open GMain



let rt = ref None (* keep a copy of last route around so expose_event can
		     redraw it *)


(* current routing stack in use *)
let stack = ref 0

let start_stop_btn = ref None
let start_stop_tab:GPack.table option ref = ref None
let ss_btn() = o2v !start_stop_btn
let ss_tab() = o2v !start_stop_tab
let choose_route_btn = ref None
let rt_btn() = o2v !choose_route_btn

(* Flags indicating what information to display. *)
let show_nodes = ref true
let show_connectivity = ref false

(* Flag indicating whether the user chooses the src by click or by 
   entering node id.*)
let text_entry = ref false  



(* Destination is 0 by default. Source is chosen by user. *)
let dst = 0
let src_ = ref 0
let src() = !src_
 
let route_portion = ref 1.0


let draw_nodes () = 
  Gui_ops.draw_all_nodes();
  Gui_ops.draw_node ~emphasize:true !src_;
  Gui_ops.draw_node ~emphasize:true dst
  


let refresh ?(clear=true) ()  = (
  Gui_gtk.clear();

  if !show_nodes  then  draw_nodes(); 
  if !show_connectivity  then  Gui_ops.draw_connectivity();

  if (!rt <> None) then
    Gui_ops.draw_grep_route 
      (Gui_conv.route_nodeid_to_pix (o2v !rt));
)


let running = ref false
let start_stop () = (
  
  match !running with
    | true -> 
	Gui_gtk.txt_msg "Nodes are frozen ";
	Mob_ctl.stop_all();
	Gui_ctl.stop();
	running := not !running;
	refresh ();
    | false -> 
	Gui_gtk.txt_msg "Nodes are moving ";
	Mob_ctl.start_all();
	rt := None;
	Gui_ctl.startsim ~sim_tick:1. ~rt_tick_ms:1000 ~display_cb:refresh;
	running := not !running;
)


let get_route nid = (
  Grep_hooks.reset();

  (*  (Sched.s())#run(); *)
  src_ := nid;
  
  Gui_gtk.txt_msg (Printf.sprintf "Route from %d to %d" !src_ dst);


  let routeref = (ref (Route.create())) in
  Gui_hooks.routes_done := 0;
  let in_mhook = Gui_hooks.grep_route_pktin_mhook routeref in
  let out_mhook = Gui_hooks.grep_route_pktout_mhook routeref in
  Nodes.iter (fun n -> n#clear_pkt_mhooks ());
  Nodes.iter (fun n -> n#add_pktin_mhook in_mhook);
  Nodes.iter (fun n -> n#add_pktout_mhook out_mhook);
  (Nodes.node (!src_))#originate_app_pkt ~l4pkt:`EMPTY ~dst;

  (Sched.s())#run_until 
  ~continue:(fun () -> 
    !Gui_hooks.routes_done = 1;
  );



  rt := Some !routeref;
  refresh();
  Log.log#log_notice 
    (lazy (Printf.sprintf "%d DATA xmits" !Grep_hooks.data_pkts_sent));
(*  Mob_ctl.start_all();*)
)

let choose_node () = (
  (* call Mob_ctl.stop_all always because node mobs might not be stopped even 
     when  !running is false. *)
  Mob_ctl.stop_all();
  
  if !running then (
    start_stop();
  );
  if !text_entry then
    Gui_ops.dialog_pick_node ~default:!src_ ~node_picked_cb:get_route ()
  else
    Gui_ops.user_pick_node ~msg:"Pick a node, dude" ~node_picked_cb:get_route ()
)
  
(* lifted from testgtk.ml (example comes with lablgtk) *)
let create_menu () = (
  let menu = GMenu.menu () and group = ref None in
  for i = 0 to 1 do
    let menuitem = GMenu.radio_menu_item ?group:!group
      ~label:("protostack "^(string_of_int i))
      ~packing:menu#append ~show_toggle:true
      () in
    ignore(menuitem#connect#toggled 
      ~callback:(fun () -> stack := i ));

    group := Some (menuitem #group);
  done;
  menu
)

let create_menus () = (
  let window = GWindow.window ~title: "menus"
    ~border_width: 0 () in
  ignore(window #connect#destroy ~callback:(fun _ -> ()));
  ignore(window #event#connect#delete ~callback:(fun _ -> true));

  let box2 = GPack.vbox ~packing:window#add () in
  
  let menu = create_menu() in
  
  let optionmenu = GMenu.option_menu ~packing: box2#add () in
  optionmenu #set_menu menu;
  optionmenu #set_history 3;

  window#show ()

)

let refresh_fun () = ignore (refresh())

let buttonlist = [
  ("Move nodes",       `TOGGLE,  [`FUNC start_stop]);
  ("Draw route",       `TOGGLE,  [`FUNC choose_node]);
  ("Show nodes",       `CHECK,   [`TOGGLE show_nodes; `FUNC refresh_fun ]);
  ("Show Connectivity",`CHECK,   [`TOGGLE show_connectivity; `FUNC refresh_fun]);
  ("Text",            `CHECK,    [`TOGGLE text_entry; `FUNC refresh_fun]);
]


let setup_grepviz_app() = (

  let top = ref 0 in

  (* Create a table to put our buttons in. *)
  let ss_tab = (GPack.table ~rows:8 ~columns:1 ~homogeneous:false 
    ~row_spacings:0 ~col_spacings:0 ~border_width:0
    ~packing:(Gui_gtk.hpacker()) ()) in

  (* Create buttons *)
  Gui_widgets.make_buttons ss_tab buttonlist top;

  (* Create scaler (slider bar at the bottom which controls partial
     visualization of route) *)
  let adj =
    GData.adjustment ~lower:0. ~upper:1001. ~step_incr:1. ~page_incr:100. () in
  let sc = GRange.scale `HORIZONTAL ~adjustment:adj ~draw_value:false
    ~packing:(Gui_gtk.vpacker()) () in
    
  ignore (adj#connect#value_changed
    ~callback:(fun () -> 
      Printf.printf "value %f\n" adj#value; flush stdout;
      
      route_portion := 
      if       adj#value > 990.0 then 1.0 else
      adj#value/.1000.;
      if (!rt <> None) then refresh ();
    ));


  (* Tell gtk to invoke our refresh method whenever the window is exposed *)
  Gui_gtk.set_expose_event_cb (fun _ -> refresh(); false);

  create_menus();
)




    
(* to kill: window#destroy ()*)

