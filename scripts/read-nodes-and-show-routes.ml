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
init_greedy_world();
Persistency.read_state ~in_chan:(open_in_bin pers_file);

Ler_graphics.init_gfx();
Ler_graphics.clear_gfx();
ignore (gui_one_route());;

Misc.wait_for_line();;

