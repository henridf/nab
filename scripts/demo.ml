



open GMain
open Printf
open Misc
open Script_utils






(*
let resched_me = 
  let c() = Gui_ops.draw_all_nodes() in
  let r() = (Sched.s())#sched_in (fun () -> c()) 1.0 in
  let f() = begin (); 
  (c(); r(), fun () -> r())
*)


let rec clock_tick() = (
  Gui_ops.draw_all_nodes(); 
  (Sched.s())#sched_in ~f:clock_tick ~t:1.0;
)


let check_all_routes () = (
  let routeref = (ref (Route.create())) in
  Gui_hooks.route_done := false;
  let in_mhook = Gui_hooks.ease_route_pktin_mhook routeref in
  let out_mhook = Gui_hooks.ease_route_pktout_mhook routeref in
  Nodes.gpsiter (fun n -> n#clear_pkt_mhooks);
  Nodes.gpsiter (fun n -> n#add_pktin_mhook in_mhook);
  Nodes.gpsiter (fun n -> n#add_pktout_mhook out_mhook);
  Nodes.gpsiter (fun n -> 
    Printf.printf "New Source: %d\n" n#id; flush stdout;
    if n#id > (Param.get Params.ntargets) then (

      n#originate_app_pkt ~dst:0;

      (Sched.s())#run_until 
      ~continue:(fun () -> 
	Gui_hooks.route_done = ref false;
      );
      
      Printf.printf "Route:\n %s\n" (Route.sprint ( !routeref));flush stdout;
      (*  ignore (Route.route_valid !routeref ~dst:(Nodes.node 0)#pos ~src:(Nodes.node
	  srcnode)#pos);*)
    )
  )
)

let load_nodes() = (
  let in_chan = open_in ((Sys.getcwd())^"/out.mld") in
  Persistency.read_state ~in_chan;

(*  Printf.printf "LOAD|| : Prop met: %f\n" (proportion_met_nodes 1);*)
  Nodes.gpsiter (fun n ->   
    Printf.printf "LOAD||: Node %d at pos : %s\n" n#id (Coord.sprintf n#pos));
  flush stdout;
)

let save_nodes() = (
(*  Printf.printf "SAVE|| : Prop met: %f\n" (proportion_met_nodes 1);*)
  Nodes.gpsiter (fun n ->   
    Printf.printf "SAVE||: Node %d at pos : %s\n" n#id (Coord.sprintf n#pos));
  flush stdout;
  let out_chan = open_out ((Sys.getcwd())^"/out.mld") in
  Persistency.save_state ~out_chan ~ntargets:1
)

let do_one_run() = (


  init_sched();
  init_greedy_world();



  make_grease_nodes();
(*  load_nodes();*)

(*  check_all_routes();*)

  Mob_ctl.make_epfl_waypoint_mobs();
  Mob_ctl.start_all();
    

  

(*
  move_nodes ~prop:0.5 ~targets:1;
 save_nodes();
*)
(*
  let routeref = ref (Route.create()) in
  let ease_mhook = Gui_hooks.ease_route_mhook routeref in
  Nodes.iter (fun n -> n#add_pktin_mhook ease_mhook);
  Nodes.iter (fun n -> n#add_pktout_mhook ease_mhook);
  

 (Nodes.node 17)#originate_app_pkt ~dstid:0;
  (Sched.s())#run();
  Printf.printf "Route:\n %s\n" (Route.sprint ( !routeref));
  Gui_ops.draw_route (Gui_hooks.mtr_2_pix_route !routeref);
*)
(*  (Sched.s())#run_until (fun () -> (Time.get_time()) < start_time +. 100.0);
  (Sched.s())#sched_in ~handler:clock_tick ~t:1.0;*)

(*  (Sched.s())#stop_in 20.0;*)

  let avgn = avg_neighbors_per_node() in
  let end_time = Time.get_time() in
  Printf.fprintf stderr "Avg neighbors per node is %f\n" avgn;
  
  flush stderr
)

let _ = 
  Read_coords.make_graph();
  Read_coords.check_ngbrs();
  
  (*  Read_coords.check_conn();*)
  Param.set Params.nodes 100;
  Param.set Params.rrange 20.0;
  Param.set Params.x_size 800.0;
  Param.set Params.y_size 600.0;
  Gui_gtk.init ();
  Log.set_log_level ~level:Log.LOG_DEBUG;
  Gui_ctl.create_buttons_ease();
  Gui_ops.draw_all_boxes();
  do_one_run();


  Main.main();

