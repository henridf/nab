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


let nstacks = 3

let routes = Array.init nstacks (fun _ -> (Route.create()))
let clear_routes() =  Array.iteri (fun i _ -> routes.(i) <- (Route.create())) routes

(* Flags indicating what information to display. *)
let show_nodes = ref true 
let show_route_lines = ref true
let show_route_anchors = ref true
let show_route_disks = ref true
let show_connectivity = ref false

(* Flag indicating whether the user chooses the src by click or by 
   entering node id.*)
let text_entry = ref false  


(* Are we displaying the EASE or the GREASE route ? *)
let proto = ref `GREASE
let stack_of_proto() = match !proto with `FRESH -> 0 | `EASE -> 1 | `GREASE -> 2

(* Destination is 0 by default. Source is chosen by user. *)
let dst = 0

let src = ref 1 (* source will be chosen by user *)



let running = ref false

let route_portion = ref 1.0

let draw_nodes () = 
  Gui_ops.draw_all_nodes();
  Gui_ops.draw_node ~emphasize:true dst

let refresh ()  = (

  Gui_gtk.clear();

  if !show_nodes  then  draw_nodes(); 
  if !show_connectivity  then  Gui_ops.draw_connectivity(); 
  
  Gui_ops.draw_ler_route 
    ~lines:!show_route_lines
    ~anchors:!show_route_anchors
    ~disks:!show_route_disks
    ~portion:!route_portion
    (Gui_conv.ler_route_nodeid_to_pix routes.(stack_of_proto()));
)


let start_stop () = (
  (* if we are in the middle of choosing a node, should we cancel all state? *)
  
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
	clear_routes();
	Gui_ctl.startsim ~sim_tick:1. ~rt_tick_ms:1000 ~display_cb:refresh;
	running := not !running;
)


let route_ctr = ref 0
let set_src nid = ( 

  src := nid;

  Gui_gtk.txt_msg (Printf.sprintf "Route from %d to %d" nid dst);
  Log.log#log_always (lazy (Printf.sprintf "Destination is at %s"
    (Coord.sprintf (Nodes.node dst)#pos)));
  
  clear_routes();

  let r = Ler_utils.get_route ~nstacks:3 ~src:nid ~dst () in

  routes.(0) <- (r.(0));
  routes.(1) <- (r.(1));
  routes.(2) <- (r.(2));

  Log.log#log_info  (lazy (Ler_route.sprintnid routes.(stack_of_proto())));
(*
  ignore (Route.ler_route_valid !routeref 
    ~dst
    ~src:nid);
*)
  
  refresh();
)



let choose_node () = (
  (* call Mob_ctl.stop_all always because node mobs might not be stopped even 
     when  !running is false. *)
  Mob_ctl.stop_all();
  
  if !running then (
    start_stop();
  );
  if !text_entry then
    Gui_ops.dialog_pick_node ~default:!src ~node_picked_cb:set_src ()
  else
    Gui_ops.user_pick_node ~msg:"Pick a node, dude" ~node_picked_cb:set_src ()
)


let refresh_fun () = ignore (refresh())

let radiobuttonlist = [
  ("GREASE", [`FUNC (fun () -> proto := `GREASE); `FUNC refresh_fun]);
  ("EASE", [`FUNC (fun () -> proto := `EASE); `FUNC refresh_fun]);
  ("FRESH", [`FUNC (fun () -> proto := `FRESH); `FUNC refresh_fun]);
]

let graph_gradient() =()

(*  let oc = open_out "/tmp/gradient_fresh.dat" in
  
  let cost nid = if nid = 0 then 0.0 else 
    let str_agent = Hashtbl.find Str_agent.agents_array_.(0) nid in
      let rt, metric = str_agent#rtab_metric in
      let ent = Str_rtab.best_invalid_entry rt metric 0 in
      Str_rtab.cost metric ent
  in
  Hashtbl.iter (fun id str_agent -> 
    let c = cost id
    and d = (World.w())#dist_nodeids 0 id in 
    if c < max_float then
      Printf.fprintf oc "%.2f %.2f\n" d c)
    Str_agent.agents_array_.(0);
  close_out oc;
  if !rt <> None then (
    let oc = open_out "/tmp/route_fresh.dat" in
    List.iter (fun h -> 
      let nid = h.Route.hop in 
      let c = cost nid 
      and d = (World.w())#dist_nodeids 0 nid in 
      assert (c < max_float);
      if c < max_float then
	Printf.fprintf oc "%.2f %.2f\n" d c
    ) (List.rev (o2v !rt));
    close_out oc;
    let oc = open_out "/tmp/floods_fresh.dat" in
    List.iter (fun h -> 
      begin 
	match h.Route.info with
	| None -> ()
	| Some flood -> 
	    let cost = cost (NaryTree.root flood) in
	    let flood_span = ref 0.0 in 
	    NaryTree.iter 
	      (fun nid -> if (World.w())#dist_nodeids 0 nid > !flood_span then 
		flood_span := (World.w())#dist_nodeids 0 nid) 
	      flood;
	    let d_root_dest = 
	      ((World.w())#dist_nodeids 0 (NaryTree.root flood)) in
	    let lower_bar =  -. !flood_span
	    and upper_bar =  !flood_span in
	    if cost < max_float then
	    Printf.fprintf oc "%.2f %.2f %.2f %.2f\n"
	      d_root_dest
	      cost 
	      (lower_bar +. cost)
	      (upper_bar +. cost)
      end;     
    ) (List.rev (o2v !rt));
    close_out oc
  )
*)
let buttonlist = [
  ("Move nodes",      `TOGGLE,  [`FUNC start_stop]);
  ("Draw route",      `TOGGLE,  [`FUNC choose_node]);
  ("Hide nodes",      `CHECK,   [`TOGGLE show_nodes; `FUNC refresh_fun ]);
  ("Hide Anchors",    `CHECK,   [`TOGGLE show_route_anchors; `FUNC refresh_fun]);
  ("Hide Directions", `CHECK,   [`TOGGLE show_route_lines; `FUNC refresh_fun]);
  ("Hide Disks",      `CHECK,   [`TOGGLE show_route_disks; `FUNC refresh_fun]);
  ("Text",            `CHECK,   [`TOGGLE text_entry; `FUNC refresh_fun]);
  ("Graph Gradient",  `TOGGLE,  [`FUNC graph_gradient]);
] 


let setup_easeviz_app() = (

  let top = ref 0 in

  (* Create a table to put our buttons in. *)
  let ss_tab = (GPack.table ~rows:8 ~columns:1 ~homogeneous:false 
    ~row_spacings:0 ~col_spacings:0 ~border_width:0
    ~packing:(Gui_gtk.hpacker()) ()) in

  (* Create buttons *)
  Gui_widgets.make_buttons ss_tab buttonlist top;
  Gui_widgets.make_radio_buttons ss_tab radiobuttonlist top;

  (* Create scaler (slider bar at the bottom which controls partial
     visualization of route) *)
  let adj =
    GData.adjustment ~lower:0. ~upper:1001. ~step_incr:1. ~page_incr:100. () in
  let sc = GRange.scale `HORIZONTAL ~adjustment:adj ~draw_value:false
    ~packing:(Gui_gtk.vpacker()) () in
    
  ignore (adj#connect#value_changed
    ~callback:(fun () -> 
      route_portion := 
      if       adj#value > 990.0 then 1.0 else
      adj#value/.1000.;
      ignore (refresh());
    ));


  let sep = GMisc.separator `HORIZONTAL ~packing:(Gui_gtk.vpacker()) ~height:4
    () in

  (* Tell gtk to invoke our refresh method whenever the window is exposed *)
  Gui_gtk.set_expose_event_cb (fun _ -> refresh(); false);

)


   
(* to kill: window#destroy ()*)

