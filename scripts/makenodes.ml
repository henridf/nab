



open Printf
open Misc
open Script_utils


let _ = 

  let mob = new Mobs.randomWalk in
  let n_nodes = 1000 in
  let pers_file = Printf.sprintf 
  "warmup/mws-%dn-0.5w-%s.mld" n_nodes mob#abbrevname in

  

  Log.set_log_level ~level:Log.LOG_NOTICE;
  Param.set Params.nodes n_nodes;
  init_sched();
  init_greedy_world();
  set_tracefile "warmup/naml-trace.mld";
  make_bler_nodes();
  
  mob#initialize();
  move_nodes ~f:mob#move ~percent:0.5;

  Persistency.save_state ~node_cnt:n_nodes ~out_chan:(open_out_bin pers_file);
  
  cleanup();

