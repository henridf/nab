(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils

let reinit() = (
  Common.set_time 0.0;
  init_sched();
  init_greedy_world();
)

let targets n = match n with 
  | 500 -> 500
  | 1000 -> 320
  | 2000 -> 160
  | 4000 -> 80
  | 8000 -> 40
  | 16000 -> 20
  | 32000 -> 10
  | 64000 -> 5
  | n -> n

let nw_sizes = [
(*  500;*)
  1000;
(*  2000;
  4000;
  8000;
  16000;
  32000;
  64000;*)
]
let mobs = [
  (*  new Mob.randomWalk;
      new Mob.waypoint;*)
  new Mob.randomJump
]

let do_one_run n_nodes mob = (
  Param.set Params.nodes n_nodes;
  reinit();
  mob#initialize();
  let nt = targets n_nodes in
  let pers_file = Printf.sprintf 
    "warmup/mws-%dn-0.4w-%dt-%s.mld" n_nodes nt mob#abbrevname in
  make_bler_nodes ?ntargets:(Some nt) ();
  move_nodes ~f:mob#move ~percent:0.4 ~targets:nt;
  Persistency.save_state ~node_cnt:n_nodes ~out_chan:(open_out_bin pers_file) ~ntargets:nt;
)

let _ = 
  Log.set_log_level ~level:Log.LOG_NOTICE;

  List.iter 
    (fun n ->  List.iter 
      (fun mob -> 

	Printf.printf "Mob %s : doing %d nodes \n" mob#abbrevname n;
	flush stdout;
	do_one_run n mob) 
      mobs) 
    nw_sizes;
  cleanup();
  
