(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)

open Printf
open Misc
open Script_utils



let () = 

  
  Warmup_utils.setup_or_restore();

  Pervasives.at_exit (fun () ->
    let stats = Warmup_utils.get_added_stats() in
    print_string "\n\n";
    print_string stats;
  );


  Warmup_utils.maybe_warmup();

  Pervasives.at_exit (fun () ->
    let stats = Warmup_utils.get_added_stats() in
    print_string "\n\n";
    print_string stats;

  );

 
  (*  install_tsources();  
      
      let pkts_origd() = (Str_agent.total_stats()).Str_agent.Str_stats.data_orig in
      
      while (true) do
      (Sched.s())#run_for ~duration:100.;
      if pkts_origd() > (Param.get Config.pktssend) then exit 0
      done;
  *)


  let pkts_origd() = (Str_agent.total_stats()).Str_agent.Str_stats.data_orig in

  while (true) do
    let src = Random.int (Param.get Params.nodes) in
    Mob_ctl.stop_all();
    (Nodes.node src)#originate_app_pkt ~l4pkt:`EMPTY ~dst:1999;
    (Sched.s())#run_for ~duration:15.;
    Mob_ctl.start_all();
    (Sched.s())#run_for ~duration:20.;
    Hashtbl.iter (fun id agent -> 
      let rt, metric = agent#rtab_metric in
      Str_rtab.purge_n_hop_entries rt metric 1999)
      Str_agent.agents_array_.(0);
    if pkts_origd() > (Param.get Traffic_utils.TParams.pkts_orig) then exit 0
  done;





