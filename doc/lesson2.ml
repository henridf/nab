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




let nodes = 800
let rrange = 12.0
let avg_degree = 8

let in_pkt_counts = [|0; 0|]
let out_pkt_counts = [|0; 0|]
let nstacks = 2

let setup() = (
  Param.set Params.nodes nodes;

  Param.set Params.radiorange rrange;

  let size = Script_utils.size ~rrange ~nodes ~avg_degree () in

  Param.set Params.x_size size;
  Param.set Params.y_size size;

  Script_utils.init_lazy_world();

  Script_utils.make_naked_nodes ();
  Script_utils.install_null_macs ~stack:0 ();
  Script_utils.install_contention_macs ~stack:1 ();

  Script_utils.make_flood_agents ~stack:0 ();
  Script_utils.make_flood_agents ~stack:1 ();
)

let pktin_hook stack pkt node = 
  in_pkt_counts.(stack) <- in_pkt_counts.(stack) + 1
let pktout_hook stack pkt node = 
  out_pkt_counts.(stack) <- out_pkt_counts.(stack) + 1

let setup_hooks() = (

  for stack = 0 to nstacks - 1 do 
    begin
      Nodes.iter (fun n -> n#clear_pkt_mhooks ~stack ());
      Nodes.iter (fun n -> n#add_pktout_mhook ~stack (pktout_hook stack));
      Nodes.iter (fun n -> n#add_pktin_mhook ~stack (pktin_hook stack) );
    end
done
)


let do_flood() = (
  let center_x = (Param.get Params.x_size) /. 2.0
  and center_y = (Param.get Params.y_size) /. 2.0  in
  let originator = 
    Opt.get ((World.w())#find_closest ~pos:(center_x, center_y) ())
  in
  (Nodes.node originator)#originate_app_pkt ~dst:L3pkt.l3_bcast_addr;
  (Sched.s())#run()
)

let print_info() = (
  let avg_ngbrs = Script_utils.avg_neighbors_per_node() 
  in 
  Log.log#log_always (lazy (Printf.sprintf "Avg degree is %f" avg_ngbrs));
  for stack = 0 to nstacks - 1 do 
    Log.log#log_always (lazy 
      (Printf.sprintf "Stack %d: TX %d packets, RX %d packets" 
	stack out_pkt_counts.(stack) in_pkt_counts.(stack)));
  done;

  let collisions = ref 0 in
  let contmacs = Mac_contention.macs ~stack:1 () in
  let getcolls mac = 
    let stats = mac#other_stats in
    stats.Mac_contention.collisions
  in
  Hashtbl.iter 
    (fun nid mac -> collisions := !collisions + (getcolls mac))  contmacs;
  Log.log#log_always 
    (lazy 
      (Printf.sprintf "%d Collisions on contention MAC (stack 1)" !collisions))
)


let main = 
  Log.set_log_level Log.LOG_NOTICE;
  setup();
  setup_hooks();
  do_flood();
  print_info()

