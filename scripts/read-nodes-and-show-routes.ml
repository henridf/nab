(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils

let targets n = match n with 
  | 500 -> 500
  | 1000 -> 1000
  | 2000 -> 160
  | 4000 -> 80
  | 8000 -> 40
  | 16000 -> 20
  | 32000 -> 10
  | 64000 -> 5
  | n -> n

let _ = 
  let n_nodes = 1000 in
  let pers_file = Printf.sprintf
    "warmup/mws-%dn-0.4w-%dt-rj.mld" n_nodes (targets n_nodes) in

Log.set_log_level ~level:Log.LOG_NOTICE;
set_tracefile "naml-trace.mld";
Param.set Params.nodes n_nodes;
init_sched();
init_world();
Persistency.read_state ~in_chan:(open_in_bin pers_file);

Ler_graphics.init_gfx();
Ler_graphics.clear_gfx();
ignore (gui_one_route());;

Misc.wait_for_line();;

