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
  and rrange = (Param.get Params.radiorange)
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


(*  init_epfl_world();*)
  init_lazy_world();

  begin match agenttype with
    | AODV -> make_aodv_nodes()
    | STR -> make_str_nodes Str_rtab.AODV;
    | GREP -> make_grep_nodes();
	Hashtbl.iter (fun _ grep_agent -> grep_agent#start_hello ())
	  (Grep_agent.agents ())
  end;

(*  Mob_ctl.make_epfl_waypoint_mobs();*)
  Mob_ctl.make_billiard_mobs ~gran:(rrange /. 10.) ();
  Mob_ctl.set_speed_mps speed;
(*  Mob_ctl.start_all();*)

  print_degree();


)

let () = 

  Param.set Params.x_pix_size 600;
  Param.set Params.y_pix_size 600;

  Param.set Params.radiorange rrange;

  Param.set Params.mac "null";

  Script_utils.parse_args();



  do_one_run();


  Mob_ctl.start_all();



  (Sched.s())#run_for ~duration:400.;
(*  Mob_ctl.stop_all();*)

  Hashtbl.iter (fun id str_agent -> if id <> 0 then str_agent#stop_hello) Str_agent.agents_array_.(0);
  for i = 0 to 1000 do
    let src = Random.int (Param.get Params.nodes) 
    and dst = Random.int (Param.get Params.nodes) in

    (Nodes.node src)#originate_app_pkt ~l4pkt:`EMPTY ~dst:0;
    (Sched.s())#run_for ~duration:40.;    
  done;

  let module S = Str_agent.Str_stats in
  let data_recv = 
    (Hashtbl.find Str_agent.agents_array_.(0) 0)#stats.S.data_recv in
  let total_xmit = 
    (Hashtbl.fold 
      (fun id agent tot -> tot + agent#stats.S.total_xmit)
      Str_agent.agents_array_.(0)
      0)
  in 
  Log.log#log_always (lazy (sprintf "Node 0 received %d data packets" data_recv));
  Log.log#log_always (lazy (sprintf "Total packet transmissions is %d packets" total_xmit));

(*  Log.set_log_level Log.LOG_DEBUG;*)
  (* Mob_ctl.stop_all();*)


  (* (Sched.s())#run();
  install_tsources();
  (Sched.s())#run_for ~duration:3000.;
  exit 0;

  let dst = 0 in
  for i = 0 to -1 do 
    (Nodes.node dst)#originate_app_pkt ~l4pkt:`EMPTY ~dst:(i + 2);

  done;
  *)
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


