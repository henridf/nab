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

let detach = Param.stringcreate 
  ~name:"detach" 
  ~doc:"Detach from terminal"
  ~cmdline:true
  ~default:""
  ~notpersist:true
  ()
  


let () = 

  Script_utils.parse_args();
  Arg.current := 0;
  if Param.get detach <> "" then 
    Script_utils.detach_daemon ~outfilename:(Param.get detach) ();


  Warmup_utils.setup_or_restore();

  Pervasives.at_exit (fun () ->
    let stats = Warmup_utils.get_added_stats() in
    output_string !Log.ochan "\n\n";
    output_string !Log.ochan stats;
  );

  (*
    install_tsources();  
    
    let pkts_origd() = (Str_agent.total_stats()).Str_agent.Str_stats.data_orig in
    
    while (true) do
    (Sched.s())#run_for ~duration:100.;
    if pkts_origd() > (Param.get Config.pktssend) then exit 0
    done;
  *)

  let dsts = Traffic_utils.all_destinations() in
  if List.length dsts <> 1 then 
    failwith "Expecting one destination";
  let dst = List.hd dsts in

  let pkts_origd() = (Str_agent.total_stats()).Str_agent.Str_stats.data_orig in

  Mob_ctl.stop_all();

  while (true) do
    let src = Random.int (Param.get Params.nodes) in

    (Nodes.node src)#originate_app_pkt ~l4pkt:`EMPTY ~dst;
    (Sched.s())#run_for ~duration:30.;
    Hashtbl.iter (fun id agent -> 
      let rt, metric = agent#rtab_metric in
      Str_rtab.purge_n_hop_entries rt metric dst)
      Str_agent.agents_array_.(0);
    if pkts_origd() > (Param.get Traffic_utils.TParams.pkts_orig) then exit 0
  done;





