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


open GMain
open Misc
open Script_utils
open Grep_common

let rrange = 100.

let sp = Printf.sprintf

let encounter_ratio() = 
  let total_encounters = 
    Hashtbl.fold (fun id str_agent encs -> 
      let rt, metric = str_agent#rtab_metric in
      if  Str_rtab.best_invalid_entry rt metric 0 <> Str_pkt.null_triple then 
	encs + 1 else encs
    ) Str_agent.agents_array_.(0) 0;
  in
  (float total_encounters) /. (float ((Param.get Params.nodes)))
  
let mob_warmup() = 
  Mob_ctl.start_all();

  Hashtbl.iter (fun id str_agent -> if id <> 0 then str_agent#stop_hello) Str_agent.agents_array_.(0);

  begin try
    while true do
      (Sched.s())#run_for ~duration:((Param.get Params.x_size) /. 4.);
      let r = encounter_ratio() in
      if r > 0.4 then (
	Log.log#log_always (lazy (sp "Mob. warmup complete (encounter ratio %.2f) " r));
	raise Misc.Break
      ) else 
	Log.log#log_always (lazy (sp "Mob. warmup continuing (encounter ratio %.2f) " r));
    done;
  with Misc.Break -> ()
  end;
  
  Mob_ctl.stop_all()
    
    
let traffic_warmup() = 
  Log.log#log_always (lazy (sp "Traffic warmup "));


  Hashtbl.iter (fun id str_agent -> if id <> 0 then str_agent#stop_hello) Str_agent.agents_array_.(0);
    Mob_ctl.start_all();  
  for i = 1 to (Param.get Params.nodes) / 100 do


    let src = Random.int (Param.get Params.nodes) in
    Mob_ctl.stop_all();
    (Nodes.node src)#originate_app_pkt ~l4pkt:`EMPTY ~dst:0;
    (Sched.s())#run_for ~duration:15.;
    Mob_ctl.start_all();
    (Sched.s())#run_for ~duration:20.;
(*    Hashtbl.iter (fun id agent -> 
      let rt, metric = agent#rtab_metric in
      Str_rtab.purge_n_hop_entries rt metric 0)
      Str_agent.agents_array_.(0);*)


  done;
  Mob_ctl.stop_all();
  (Sched.s())#run_for ~duration:20.(* purge traffic still in network. *)
  


let () = 

  Param.set Params.x_pix_size 600;
  Param.set Params.y_pix_size 600;

  Param.set Params.radiorange rrange;

  Param.set Params.mac "null";

  Script_utils.parse_args();
  Param.printconfig stdout;

  setup_sim();

  Hashtbl.iter (fun id str_agent -> if id <> 0 then str_agent#stop_hello) Str_agent.agents_array_.(0);

  Pervasives.at_exit (fun () ->
    let stats = get_added_stats() in
    print_string "\n\n";
    print_string stats;

  );

  if Param.get Config.warmup <> NONE then (
    Log.log#log_always (lazy (sp "Warming up with %s" (Param.as_string Config.warmup)));
    let fname = sp "%s-%dn-%s-%s.dat" 
      (Param.as_string Config.agent)
      (Param.get Params.nodes)
      (Param.as_string Config.warmup)
      (Param.as_string Mob_ctl.mob)
    in
    Log.log#log_always (lazy (sp "Will dump to file %s" fname));
    if Sys.file_exists fname then (
      Log.log#log_always (lazy (sp "OOops! %s already exists!" fname));
      exit (-1);
    );
    begin match (Param.get Config.warmup) with
      | TRAFFIC -> mob_warmup(); traffic_warmup()
      | MOB -> mob_warmup()
      | NONE -> ()
    end;
    let oc = Pervasives.open_out fname in
    Persistency.save_node_state oc;
    Persistency.save_str_agents oc;
    close_out oc;
  ) else (*mob_warmup*)();


  (* (Sched.s())#run();
     install_tsources();
     (Sched.s())#run_for ~duration:3000.;
     exit 0;

     let dst = 0 in
     for i = 0 to -1 do 
     (Nodes.node dst)#originate_app_pkt ~l4pkt:`EMPTY ~dst:(i + 2);

     done;
  *)
  
  print_degree();

  Gui_gtk.init();
  Gui_grep.setup_grepviz_app();
  Main.main();

