



open GMain
open Printf
open Misc
open Script_utils
open Grep_common

let avg_degree = 12
let rrange = 100.


let print_degree() = 
  let avgn = avg_neighbors_per_node() in
  Log.log#log_notice (lazy (sprintf "Avg neighbors per node is %f\n" avgn))


let do_one_run() = (

  let agenttype = Config.agent_of_string (Param.get Config.agent)
  and speed = (Param.get Config.speed)
  and rrange = (Param.get Params.rrange)
  in

  Randoms.change_seed ~newseed:(Param.get Config.run) () ;

  Param.set Params.x_size 
    (size ~rrange ~avg_degree ~nodes:(Param.get Params.nodes) ());
  Param.set Params.y_size 
    (size ~rrange ~avg_degree ~nodes:(Param.get Params.nodes) ());

(*
  Param.set Params.x_size 800.;
  Param.set Params.y_size 600.;
*)


  init_sched();

(*  init_epfl_world();*)
  init_lazy_world();

  begin match agenttype with
    | AODV -> make_aodv_nodes()
    | GREP -> make_grep_nodes();
	Array.iter (fun grep_agent -> grep_agent#start_hello ())
	  !Grep_agent.agents_array
  end;

(*  Mob_ctl.make_epfl_waypoint_mobs();*)
  Mob_ctl.make_billiard_mobs ~gran:(rrange /. 10.) ();
  Mob_ctl.set_speed_mps speed;
  Mob_ctl.start_all();

  print_degree();


)

let () = 

  Param.set Params.rrange rrange;

  Param.set Params.mac "null";

  let s = Param.make_argspeclist () 
  in
  Arg.parse s (fun s -> ()) "You messed up!";

  Gui_gtk.init();

  Gui_grep.create_buttons_grep();

  do_one_run();
(*  (Sched.s())#run_for ~duration:1600.;*)


  let dst = 0 in
  for i = 0 to -1 do 
    (Nodes.node dst)#originate_app_pkt (i + 2);

  done;
(*
  let oc = open_out "/tmp/mws_node_state" in 
  Persistency.save_node_state oc;  
  let ochan = open_out "/tmp/mws_grep_state" in 
  Persistency.save_grep_agents ochan;  
  close_out ochan;



  let ichan = open_in "/tmp/mws_node_state" in 
  Persistency.read_node_state ichan ;  
  let ichan = open_in "/tmp/mws_grep_state" in 
  Persistency.read_grep_agents ichan;  

*)  
  print_degree();

  Main.main();

