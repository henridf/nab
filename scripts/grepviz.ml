(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open GMain
open Printf
open Misc
open Script_utils
open Grep_common

let avg_degree = 12
let rrange = 100.




(*
let resched_me = 
  let c() = Gui_ops.draw_all_nodes() in
  let r() = (Gsched.sched())#sched_in (fun () -> c()) 1.0 in
  let f() = begin (); 
  (c(); r(), fun () -> r())
*)


let rec clock_tick() = (
  Gui_ops.draw_all_nodes(); 
  (Gsched.sched())#sched_in ~f:clock_tick ~t:1.0;
)


let do_one_run() = (

  let agenttype = Config.agent_of_string (Param.get Config.agent)
  and sources = (Param.get Config.sources) 
  and speed = (Param.get Config.speed)
in
  Randoms.change_seed ~newseed:(Param.get Config.run) () ;

  Param.set Params.x_size 
    (size ~rrange ~avg_degree	~nodes:(Param.get Params.nodes) ());
  Param.set Params.y_size 
    (size ~rrange ~avg_degree	~nodes:(Param.get Params.nodes) ());
  
  init_sched();
  init_world();
  
  begin match agenttype with
    | AODV -> make_aodv_nodes()
    | GREP -> make_grep_nodes();
  end;

  Mob_ctl.make_waypoint_mobs ~gran:((Param.get Params.rrange) /. 10.) ();
  Mob_ctl.set_speed_mps speed;
  Mob_ctl.start_all();


  Grep_common.install_tsources();
    

  

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
  (Gsched.sched())#run();
  Printf.printf "Route:\n %s\n" (Route.sprint ( !routeref));
  Gui_ops.draw_route (Gui_hooks.mtr_2_pix_route !routeref);
*)
(*  (Gsched.sched())#run_until (fun () -> (Common.get_time()) < start_time +. 100.0);
  (Gsched.sched())#sched_in ~handler:clock_tick ~t:1.0;*)

(*  (Gsched.sched())#stop_in 20.0;*)

  let avgn = avg_neighbors_per_node() in
  let end_time = Common.get_time() in
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

