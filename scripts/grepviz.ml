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


let print_degree() = 
  let avgn = avg_neighbors_per_node() in
  Printf.fprintf stderr "Avg neighbors per node is %f\n" avgn;
  flush stderr

let do_one_run() = (

  let agenttype = Config.agent_of_string (Param.get Config.agent)
  and speed = (Param.get Config.speed)
  and rrange = (Param.get Params.rrange)
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

  Mob_ctl.make_borderwaypoint_mobs ~gran:(rrange /. 10.) ();
  Mob_ctl.set_speed_mps speed;
  Mob_ctl.start_all();
  print_degree();
)

let () = 


  Param.set Params.rrange rrange;
  

  Param.set Params.mac "null";

  let s = Param.make_argspeclist () 
  in
  Myarg.parse s (fun s -> ()) "You messed up!";

  Gui_gtk.init ();

  Gui_grep.create_buttons_grep();

  do_one_run();

  let src() = (Param.get Params.nodes) - 1 in
  let dst = 0 in
  for i = 0 to 10 do 
    (Nodes.node (src()))#originate_app_pkt ~dst;
    (Gsched.sched())#run_for ~duration:10.;
  done;
  
  
  
  (*  (Gsched.sched())#run_for ~duration:1000.;*)
  print_degree();

  Main.main();

