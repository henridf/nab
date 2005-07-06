open Ler_agent

(* unfinished! *)



(* returns the flood tree starting at node nid with given ttl. *)
let get_flood ttl nid = 
  Flood_agent.orig_ttl := ttl;
  let floodref = ref (Flood.create nid) in 
  let in_mhook = (Flood_hooks.flood_pktin_mhook floodref) in
  Nodes.iter (fun n -> n#clear_pkt_mhooks ~stack:1 ());
  Nodes.iter (fun n -> n#add_pktin_mhook ~stack:1 in_mhook);
  (Nodes.node nid)#originate_app_pkt ~l4pkt:`EMPTY ~dst:L3pkt.l3_bcast_addr;
  (Sched.s())#run_for ~duration:(2.0 *. float !Flood_agent.orig_ttl);
  !floodref
    
(* returns a list of all paths to leaves of a flood tree 
   starting at node nid with given ttl.*)
let get_paths ttl nid = 
  let f = get_flood ttl nid in
  NaryTree.paths f
    

(* returns an array of encounter ages of nodes along a path *)
let path_ages path dst = 
  let letab nid = (Ler_agent.agent ~stack:0 nid)#le_tab in
  Array.of_list (List.map (fun nid -> (letab nid)#le_time dst) path)


(* computes the age gradient along a path *)
let path_gradient dst path  = 
  let y = Array.map (Opt.default 0.) (path_ages path dst) in
  let x = Array.mapi (fun i age -> float i) y in
  let weight = Array.map 
    (function None -> 0. | Some age -> 1.) 
    (path_ages path dst) in
  let f = Gsl_fit.linear ~weight x y in
  f.Gsl_fit.c1 (* c0 is offset, c1 is slope *)
    

open Coord

let gradients : ttl:int -> orig:int -> dst:int -> (float * float) list = 

  fun ~ttl ~orig ~dst ->

    let dir nid = 
      let dstpos = (World.w())#nodepos orig
      and nidpos = (World.w())#nodepos nid in
      Coord.angle_deg (dstpos ---. nidpos) in
    let dirtodest = dir dst in
    let paths =  get_paths ttl orig in
    let gradients = List.map 
      (fun path -> (path_gradient dst path), (dir (List.hd  path)) -. dirtodest) paths in
    gradients

    
let _ =     
  let ic = Pervasives.open_in "/home/henridf/tmp/dump.nab" in
  Persistency.restore_sim ic;
  close_in ic;
  Script_utils.install_null_macs ~stack:1 ();
  Script_utils.install_flood_agents ~stack:1 ()
    

