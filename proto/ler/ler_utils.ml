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

(* creates sorted array of <distance, age> tuples*)
let dist_age_arr ?(dst=0) () = 
  let now = Time.time() in
  let a = Nodes.mapi 
    (fun nid -> 	((World.w())#dist_nodeids dst nid), (Ler_agent.agent nid)#le_tab#le_time 0) in
  let l = Array.to_list a in
  let f = Array.of_list 
    (List.map (fun (nid, opt) -> nid, 	(now -. (Opt.get opt))) (List.filter (fun (_, o) -> Opt.is_some o) l)) in
  Array.sort (fun (d1, _) (d2, _) -> compare d1 d2)  f; f;;


let route_ctr = ref 0

let get_route ?(nstacks=1) ~src ~dst() =
  Ler_hooks.routes_done := 0;
  let r = Array.init nstacks (fun _ -> ref (Route.create())) in


  for stack = 0 to nstacks - 1  do
    let in_mhook = 
      Ler_hooks.ler_route_pktin_mhook ~num:!route_ctr r.(stack) in 
    let out_mhook = 
      Ler_hooks.ler_route_pktout_mhook ~num:!route_ctr r.(stack) in
    Nodes.iter (fun n -> n#clear_pkt_mhooks ~stack ());
    Nodes.iter (fun n -> n#add_pktin_mhook ~stack in_mhook);
    Nodes.iter (fun n -> n#add_pktout_mhook ~stack out_mhook);
    
  done;
    

  (Nodes.node src)#originate_app_pkt ~l4pkt:(`APP_PKT !route_ctr) ~dst;

  (Sched.s())#run_until 
  ~continue:(fun () -> !Ler_hooks.routes_done < nstacks;);
  Array.map ( ! ) r


let proportion_met_nodes ?(stack=0) () = 
  let targets = Param.get Ler_agent.ntargets in
  let total_encounters = 
    Hashtbl.fold (fun _nid agent encs -> (agent#le_tab#num_encounters) + encs) 
      (Ler_agent.agents ~stack ()) 0
  in
  (float total_encounters) /. (float ((Param.get Params.nodes) * targets))

