(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Header *)







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

