(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)







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
  (*  new Mobs.randomWalk;
      new Mobs.waypoint;*)
  new Mobs.randomJump
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
  
