(*
 * 
 * NAB - Network in a Box Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 * Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 * Laboratory for Computer Communications and Applications (LCA), Ecole
 * Polytechnique Federale de Lausanne (EPFL), CH-1015 Lausanne, Switzerland
 * 
 * This file is part of NAB. NAB is free software; you can redistribute it
 * and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 * 
 * NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details (enclosed in the file GPL).
 * 
 *)

(* $Id$ *)


open Printf
open Misc
open Script_utils

let avg_degree = 9

type agent_type = AODV | GREP | STR_MAX | STR_AGE | STR_AODV
type warmup_type = TRAFFIC | MOB  | NONE


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
    ()

  let warmupfile = Param.stringcreate
    ~name:"warmupfile"
    ~cmdline:true
    ~default:"none"
    ~doc:"Warmup file"
    ~checker:(fun s -> ())
    ()

  let agent_of_string = function
    | "aodv" | "AODV" -> AODV
    | "str"  | "str-max" | "str_max" -> STR_MAX
    | "str-age" | "str_age" -> STR_AGE
    | "str-aodv" | "str_aodv" -> STR_AODV
    | "grep" | "GREP" -> GREP
    | _ -> raise (Failure "Invalid format for agent type")

  let string_of_agent = function
    | AODV -> "aodv"
    | STR_MAX -> "str_max" 
    | STR_AGE -> "str_age"
    | STR_AODV -> "str_aodv"
    | GREP -> "grep"


  let agent = Param.create
    ~name:"agent"
    ~cmdline:true
    ~default:STR_AGE
    ~doc:"Routing protocol"
    ~reader:agent_of_string
    ~printer:string_of_agent
    ()

      
  let run = Param.intcreate ~name:"run" ~default:1
    ~cmdline:true
    ~doc:"Run number" ()

  let speed = 
    Param.floatcreate ~name:"speed" 
      ~cmdline:true
      ~default:8.0
      ~doc:"Node Speed [m/s]"  ()

end


let print_degree() = 
  let avgn = avg_neighbors_per_node() in
  Log.log#log_notice (lazy (sprintf "Avg neighbors per node is %f\n" avgn))



let setup_sim () = (

  let agenttype = Param.get Config.agent
  and speed = (Param.get Config.speed)
  and rrange = (Param.get Params.radiorange)
  in

  Randoms.change_seed ~newseed:(Param.get Config.run) () ;

  Param.set Params.x_size 
    (size ~rrange ~avg_degree ~nodes:(Param.get Params.nodes) ());
  Param.set Params.y_size 
    (size ~rrange ~avg_degree ~nodes:(Param.get Params.nodes) ());


(*  init_epfl_world();*)
  init_lazy_world();

  begin
    match (Param.get Config.warmupfile) with
      | "none" ->
	  begin match agenttype with
	    | AODV -> make_aodv_nodes()
	    | STR_AGE -> make_str_nodes Str_rtab.STR_AGE;
	    | STR_MAX -> make_str_nodes Str_rtab.STR_MAX;
	    | STR_AODV -> make_str_nodes Str_rtab.STR_AODV;
	    | GREP -> failwith "grep not working no more"
	  end
      | f -> 
	  let ic = Pervasives.open_in f in
	  Persistency.read_node_state ic;
	  Persistency.read_str_agents ic
  end;
  (*  Mob_ctl.make_epfl_waypoint_mobs();*)
  install_mobs ~gran:(rrange /. 10.) ();
  Mob_ctl.set_speed_mps speed;
  
  print_degree();
)


let get_added_stats() = 
  match Param.get Config.agent with
    | AODV -> let tot = Aodv_agent.total_stats() in
      Aodv_agent.Aodv_stats.sprint_stats tot
    | STR_AODV | STR_MAX | STR_AGE -> let tot = Str_agent.total_stats() in
      Str_agent.sprint_stats tot
    | GREP -> ""


