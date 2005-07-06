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

let () = 

  Script_utils.parse_args();
  
  let dumpfile = 
    try 
      (Param.get Script_params.dumpfile)
    with Param.NoParamVal _ -> "/doesntexist"
  in
  
  
  Warmup_utils.setup_or_restore dumpfile;
  

  (* this was to dump d, a 
     corresponding to a list of nodes which are the most frequent anchors.
  let ic = open_in
    "/home/henridf/work/nab/proto/ler/scripts/50most_popular_nodes.dat" in
  while true do
  let nid = int_of_string (input_line ic) in
  let d = (World.w())#dist_nodeids 0 nid in
  let a = (Ler_agent.agent nid)#le_tab#le_age 0 in
  Printf.printf "%.2f %.2f\n" d a; flush stdout;
  done;
  exit 0;
*)
  Nodes.iter (fun n -> n#remove_mac ());
  Script_utils.install_cheat_macs();
  
  Log.set_log_level Log.LOG_WARNING;
  Mob_ctl.stop_all ();

  let arr = Array.make (Param.get Params.nodes) 0 in
  for i = 0 to (Param.get Params.nodes) - 1 do
    if i mod 500 = 0  then (
      prerr_int i; prerr_char ' ';
      flush stderr;
    );
    let r = Ler_utils.get_route ~nstacks:1 ~src:i ~dst:0 () in
    let prev_age = ref max_float in
    List.iter (fun {Route.hop=hop; Route.info=anchor_opt}  ->
      let anchor = Opt.get anchor_opt in
      if anchor.Ler_route.searchcost <> 0. && hop <> i then (
	arr.(hop) <- arr.(hop) + 1
      )
    ) r.(0);
  done;
  
  prerr_newline();
  let arr_withnids = Array.mapi (fun nid pop -> pop, nid) arr in
  (* after we sort in decreasing  popularity, we can't use array index as nid. *)
  Array.sort (fun a b -> compare (fst b) (fst a)) arr_withnids; 

  let tot = float (Array.fold_right (fun v tot -> v + tot) arr 0) in

  let sum = ref 0. in
  
  
  Array.iteri (fun _ (ntimes, nid) -> 
    sum := !sum +. (float ntimes);
    if ntimes <> 0 then Printf.printf "%d %d %f\n"  nid ntimes (!sum /. tot)) arr_withnids
    
