(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open GMain
open Printf
open Misc
open Script_utils






(*
let resched_me = 
  let c() = Gui_ops.draw_all_nodes() in
  let r() = (Gsched.sched())#sched_in (fun () -> c()) 1.0 in
  let f() = begin (); 
  (c(); r(), fun () -> r())
*)


let rec clock_tick() = (
  Gui_ops.draw_all_nodes(); 
  (Gsched.sched())#sched_in ~handler:clock_tick ~t:1.0;

)

let do_one_run() = (

  let mob = new Mob.waypoint in
  Log.set_log_level ~level:Log.LOG_NOTICE;
  init_sched();
  init_world();
  mob#initialize();
  make_grease_nodes();
    
  Gui_hooks.attach_mob_hooks();

  Nodes.iter (fun n ->
    n#set_speed_mps 10.0;
    n#setmob mob#getnewpos;
    n#selfmove;
  );
  
  let start_time = Common.get_time() in
  (Gsched.sched())#sched_in ~handler:clock_tick ~t:1.0;

(*  (Gsched.sched())#stop_in 20.0;*)

  let avgn = avg_neighbors_per_node() in
  let end_time = Common.get_time() in
  Printf.fprintf stderr "Avg neighbors per node is %f\n" avgn;
  
  flush stderr
)



let _ = 
  Read_coords.make_graph();
  Param.set Params.nodes 10;
  Param.set Params.rrange 250.0;
  Param.set Params.x_size 4000.0;
  Param.set Params.y_size 3000.0;

  Gui_gtk.init ();
  Gui_ctl.create_buttons();
  Gui_ops.draw_all_nodes();
  Gui_ops.draw_all_boxes();
  do_one_run();
  Main.main();    
