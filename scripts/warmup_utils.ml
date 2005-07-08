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

let avg_degree = 9
type warmup_type = TRAFFIC | MOB  | NONE
type agent_type = AODV | STR_MAX | STR_AGE | STR_AODV | LER_FRESH | LER_GREASE | LER_EASE


module Config = 
struct
  let warmup_of_string = function
    | "traffic" | "traf" -> TRAFFIC
    | "mob" -> MOB
    | "none" -> NONE
    | _ -> raise (Failure "Invalid format for warmup type")

  let string_of_warmup = function
    | TRAFFIC -> "traffic"
    | MOB -> "mob"
    | NONE -> "none"

  let warmup = Param.create
    ~name:"warmup"
    ~cmdline:true
    ~default:NONE
    ~doc:"Simulation Warmup type"
    ~reader:warmup_of_string 
    ~printer:string_of_warmup
    ~notpersist:true
    ()

  let move_of_string = function
    | "all" -> `ALL
    | "dests" -> `DESTS
    | "sources" -> `SOURCES
    | "allbutdests" -> `ALLBUTDESTS
    | "allbutsources" -> `ALLBUTSOURCES
    | _ -> raise (Failure "Invalid format for move type")

  let string_of_move = function
    | `ALL -> "all"
    | `DESTS -> "dests"
    | `SOURCES -> "sources"
    | `ALLBUTDESTS -> "allbutdests"
    | `ALLBUTSOURCES -> "allbutsources" 
    | _ -> raise (Failure "Invalid format for move type")

  let move = Param.create
    ~name:"move"
    ~cmdline:true
    ~default:`ALL
    ~doc:"Which nodes are moving."
    ~reader:move_of_string 
    ~printer:string_of_move
    ~notpersist:true
    () 

  let agent_of_string = function
    | "aodv" | "AODV" -> AODV
    | "str"  | "str-max" | "str_max" -> STR_MAX
    | "str-age" | "str_age" -> STR_AGE
    | "str-aodv" | "str_aodv" -> STR_AODV
    | "ler_fresh" | "ler-fresh" -> LER_FRESH
    | "ler_ease" | "ler-ease" -> LER_EASE
    | "ler_grease" | "ler-grease" -> LER_GREASE
    | _ -> raise (Failure "Invalid format for agent type")

  let string_of_agent = function
    | AODV -> "aodv"
    | STR_MAX -> "str_max" 
    | STR_AGE -> "str_age"
    | STR_AODV -> "str_aodv"
    | LER_FRESH -> "ler-fresh"
    | LER_GREASE -> "ler-grease" 
    | LER_EASE -> "ler-grease" 

  let agent = Param.create
    ~name:"agent"
    ~cmdline:true
    ~default:LER_FRESH
    ~doc:"Routing protocol"
    ~reader:agent_of_string
    ~printer:string_of_agent
    ()

    
  let run = Param.intcreate ~name:"run" ~default:1
    ~cmdline:true
    ~checker:(fun i -> Randoms.change_seed ~newseed:i ())
    ~doc:"Run number" ()

end

let sp = Printf.sprintf

let sprint_added_stats() = 
  match Param.get Config.agent with
    | AODV -> let tot = Aodv_agent.total_stats() in
      Aodv_agent.Aodv_stats.sprint_stats tot
    | STR_AODV | STR_MAX | STR_AGE -> let tot = Str_agent.total_stats() in
      Str_agent.sprint_stats tot
    | LER_FRESH | LER_EASE | LER_GREASE -> ""

let sprint_added_jdbstats() = 
  match Param.get Config.agent with
    | AODV -> ""
    | STR_AODV | STR_MAX | STR_AGE -> 
	let tot = Str_agent.total_stats() in
	Str_agent.sprint_jdbstats tot
    | LER_FRESH | LER_EASE | LER_GREASE -> ""

let encounter_ratio() = 
  match Param.get Config.agent with
      STR_AGE | STR_MAX | STR_AODV ->
	let total_encounters = 
	  Hashtbl.fold (fun id str_agent encs -> 
	    let rt, metric = str_agent#rtab_metric in
	    if  Str_rtab.best_invalid_entry rt metric 0 <> Str_pkt.null_triple then 
	      encs + 1 else encs
	  ) Str_agent.agents_array_.(0) 0;
	in
	(float total_encounters) /. (float ((Param.get Params.nodes)))
    | LER_FRESH | LER_EASE | LER_GREASE -> 
	Ler_utils.proportion_met_nodes()
    | AODV -> failwith "cannot compute encounter ratio"



let set_hellos() = 
  let agenttype = Param.get Config.agent in
  if (agenttype == STR_AGE || agenttype == STR_MAX || agenttype == STR_AODV) then
    let dests = Traffic_utils.all_destinations() in
    Hashtbl.iter 
      (fun id agent -> 
	if List.mem id dests then agent#start_hello else agent#stop_hello
      ) Str_agent.agents_array_.(0)


      
let start_appropriate_nodes() = 
  let center = 
    ((Param.get Params.x_size)/. 2., (Param.get Params.y_size)/. 2.) in

  let dests = Traffic_utils.all_destinations() 
  and sources = Traffic_utils.all_sources() in
  match Param.get Config.move with
    | `ALL -> 
	Nodes.iteri (fun nid node -> Mob_ctl.start_node nid);
    | `DESTS -> 
	Nodes.iteri (fun nid node -> if (List.mem nid dests) then
	  Mob_ctl.start_node nid);
    | `SOURCES -> 
	Nodes.iteri (fun nid node -> if (List.mem nid sources) then
	  Mob_ctl.start_node nid);
    | `ALLBUTDESTS ->
	Nodes.iteri (fun nid node -> if not (List.mem nid dests) then
	  Mob_ctl.start_node nid);
	(World.w())#movenode (List.hd dests) ~newpos:center;
    | `ALLBUTSOURCES ->
	Nodes.iteri (fun nid node -> if not (List.mem nid sources) then
	  Mob_ctl.start_node nid);
    | _ -> raise (Failure "Invalid format for move type")


let mob_warmup() = 

  start_appropriate_nodes();
  set_hellos();
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

  set_hellos();
  start_appropriate_nodes();


  for i = 1 to (Param.get Params.nodes) / 100 do

    let src = Random.int (Param.get Params.nodes) in
    Mob_ctl.stop_all();
    (Nodes.node src)#originate_app_pkt ~l4pkt:`EMPTY ~dst:0;
    (Sched.s())#run_for ~duration:15.;
    start_appropriate_nodes();

    (Sched.s())#run_for ~duration:20.;
    (*    Hashtbl.iter (fun id agent -> 
	  let rt, metric = agent#rtab_metric in
	  Str_rtab.purge_n_hop_entries rt metric 0)
	  Str_agent.agents_array_.(0);*)
  done;
  Mob_ctl.stop_all();
  (Sched.s())#run_for ~duration:20.(* purge traffic still in network. *)

let print_degree() = 
  let avgn = Script_utils.avg_neighbors_per_node() in
  Log.log#log_notice (lazy (sp "Avg neighbors per node is %f" avgn))

let setup_sim () = 
  let agenttype = Param.get Config.agent
  and rrange = (Param.get Params.radiorange)
  in
  
  Param.set Params.x_size 
    (Script_utils.size ~rrange ~avg_degree ~nodes:(Param.get Params.nodes) ());
  Param.set Params.y_size 
    (Script_utils.size ~rrange ~avg_degree ~nodes:(Param.get Params.nodes) ());

  if agenttype == LER_FRESH || 
    agenttype == LER_EASE || 
    agenttype == LER_GREASE then
      Param.set World.world (World.Greedy, snd (Param.get World.world));
  (* change greediness but keep dimension *)

  Script_utils.init_all();

  begin match agenttype with
    | AODV -> 
	Script_utils.make_aodv_nodes()

    | STR_AGE -> 
	Script_utils.make_str_nodes Str_rtab.STR_AGE;
    | STR_MAX -> 
	Script_utils.make_str_nodes Str_rtab.STR_MAX;
    | STR_AODV -> 
	Script_utils.make_str_nodes Str_rtab.STR_AODV;

    | LER_FRESH -> 
	Script_utils.make_nodes();
	Script_utils.install_ler_agents Ler_agent.FRESH 
    | LER_EASE -> 
	Script_utils.make_nodes(); 
	Script_utils.install_ler_agents Ler_agent.EASE
    | LER_GREASE -> 
	Script_utils.make_nodes(); 
	Script_utils.install_ler_agents Ler_agent.GREASE
  end;
  
  Script_utils.install_mobs ()

    
let restore_sim fname = 
  let ic = Pervasives.open_in fname in
  Persistency.restore_sim ic;
  close_in ic


    
    
let maybe_warmup fname = 
  if Param.get Config.warmup <> NONE then (
    Log.log#log_always (lazy (sp "Warming up with %s" (Param.as_string Config.warmup)));
    Log.log#log_always (lazy (sp "Will dump to file %s" fname));
    if Sys.file_exists fname then (
      Log.log#log_always (lazy (sp "OOops! %s already exists!" fname));
      exit (-1);
    );
    begin match (Param.get Config.warmup) with
      | TRAFFIC -> 
	  mob_warmup(); 
	  traffic_warmup()
      | MOB -> mob_warmup()
      | NONE -> ()
    end;
    let oc = Pervasives.open_out fname in
    Persistency.save_sim oc;
    close_out oc;
  )
