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


let load_nodes() = (
  let in_chan = open_in "/home/henri/newwork/caml/out.mld" in
  Persistency.read_state ~in_chan;

  Printf.printf "LOAD|| : Prop met: %f\n" (proportion_met_nodes 1);
  Nodes.iter (fun n ->   
    Printf.printf "LOAD||: Node %d at pos : %s\n" n#id (Coord.sprintf n#pos));
  flush stdout;
)

let save_nodes() = (
  Printf.printf "SAVE|| : Prop met: %f\n" (proportion_met_nodes 1);
  Nodes.iter (fun n ->   
    Printf.printf "SAVE||: Node %d at pos : %s\n" n#id (Coord.sprintf n#pos));
  flush stdout;
  let out_chan = open_out "/home/henri/newwork/caml/out.mld" in
  Persistency.save_state ~out_chan ~ntargets:1
)

let do_one_run() = (


  Log.set_log_level ~level:Log.LOG_DEBUG;
  init_sched();
  init_world();



  make_grease_nodes();


(*  load_nodes();*)
  Gui_hooks.attach_mob_hooks();


  Mob.make_epfl_mobs();
  Mob.start_all();
    

  
(*
  move_nodes ~prop:0.4 ~targets:1;
  

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
  Param.set Params.nodes 1000;
  Param.set Params.rrange 50.0;
  Param.set Params.x_size 800.0;
  Param.set Params.y_size 600.0;
  Gui_gtk.init ();
  Gui_ctl.create_buttons();
  Gui_ops.draw_all_nodes();
  Gui_ops.draw_all_boxes();
  do_one_run();
(*  (Gworld.world())#find_closest ~pos:(10.0, 10.0) ~f:(fun _ -> true);;    *)
  
  Main.main();

