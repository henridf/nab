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


let set_debug_level s = 
  let level = 
    match s with 
      | "debug" -> Log.LOG_DEBUG
      | "notice" -> Log.LOG_NOTICE
      | "warning" -> Log.LOG_WARNING
      | "error" -> Log.LOG_ERROR
      | _ -> raise (Failure "Bad debug level argument")
  in
  Log.set_log_level level


let parse_args ?(extra_argspec=[]) ?(anon_fun=(fun _ -> ())) () = (
  let s = Param.argspeclist () in
  Arg.parse (s@extra_argspec) anon_fun "";
)

let init_sched() = Sched.makeSchedHeap()

let init_greedy_world_2d() = 
  World.set_greedy_world (new Crworld.greedy_reflecting_world
    ~x:(Param.get Params.x_size)
    ~y:(Param.get Params.y_size)
    ~rrange:(Param.get Params.radiorange))

let init_greedy_taurus_world_2d() = 
  World.set_greedy_world (new Crworld.greedy_taurus_world
    ~x:(Param.get Params.x_size)
    ~y:(Param.get Params.y_size)
    ~rrange:(Param.get Params.radiorange))

let init_epfl_world() = (
  if (Param.get Params.x_size) <> 800. ||
    (Param.get Params.y_size) <> 600. then
      Log.log#log_warning (lazy "For EPFL, size should be 800x600");
  World.set_lazy_world (new Crworld.epflworld
    ~x:(Param.get Params.x_size)
    ~y:(Param.get Params.y_size)
    ~rrange:(Param.get Params.radiorange))
)
  
let init_lazy_world_2d() = 
  World.set_lazy_world (new Crworld.lazy_reflecting_world
    ~x:(Param.get Params.x_size)
    ~y:(Param.get Params.y_size)
    ~rrange:(Param.get Params.radiorange)
)

let init_lazy_taurus_world_2d() = 
  World.set_lazy_world (new Crworld.lazy_taurus_world
    ~x:(Param.get Params.x_size)
    ~y:(Param.get Params.y_size)
    ~rrange:(Param.get Params.radiorange)
)

let init_greedy_world_1d() = 
  World.set_greedy_world (new Onedim_world.greedy_reflecting_world
    ~x:(Param.get Params.x_size) ~rrange:(Param.get Params.radiorange))

let init_greedy_ring_world_1d() = 
  World.set_greedy_world (new Onedim_world.greedy_taurus_world
    ~x:(Param.get Params.x_size)
    ~rrange:(Param.get Params.radiorange))

let init_lazy_world_1d() = 
  World.set_lazy_world (new Onedim_world.lazy_reflecting_world
    ~x:(Param.get Params.x_size)
    ~rrange:(Param.get Params.radiorange)
)

let init_lazy_ring_world_1d() = 
  World.set_lazy_world (new Onedim_world.lazy_taurus_world
    ~x:(Param.get Params.x_size)
    ~rrange:(Param.get Params.radiorange)
)

let init_world() = 
  match snd (Param.get World.world) with
    | World.One -> 
	begin match fst (Param.get World.world) with
	  | World.Greedy -> init_greedy_world_1d()
	  | World.Lazy -> init_lazy_world_1d()
	  | World.Lazy_taurus -> init_lazy_ring_world_1d()
	  | World.Greedy_taurus -> init_greedy_ring_world_1d()
	  | World.Epfl -> failwith "Script_utils.init_world : epfl is two-dimensional!"
	end
    | World.Two -> 
	begin match fst (Param.get World.world) with
	  | World.Greedy -> init_greedy_world_2d()
	  | World.Lazy -> init_lazy_world_2d()
	  | World.Lazy_taurus -> init_lazy_taurus_world_2d()
	  | World.Greedy_taurus -> init_greedy_taurus_world_2d()
	  | World.Epfl -> init_epfl_world()
	end
	      
let init_all() =   (
  Time.set_time 0.0;
  init_sched(); 
  init_world()
)

let install_macs_ ~stack mac_factory = 
  Nodes.iter (fun n -> n#install_mac ~stack (mac_factory n))

let default_bps = 1e6

let install_null_macs ?(stack=0) ?(bps=default_bps) () = 
  let makemac node = ((new Mac_null.nullmac ~stack ~bps node) :> Mac.t) in
  install_macs_ ~stack makemac

let install_queue_null_macs ?(stack=0) ?(queuesize=2) ?(bps=default_bps) () = 
  let makemac node = ((new Mac_null_queue.nullmac_q ~stack ~queuesize ~bps node) :> Mac.t) in
  install_macs_ ~stack makemac

let install_queue_cts_macs ?(stack=0) ?(queuesize=2) ?(bps=default_bps) () = 
  let makemac node = ((new Mac_cts_queue.ctsmac_q ~stack ~queuesize ~bps node) :> Mac.t) in
  install_macs_ ~stack makemac

let install_contention_macs ?(stack=0) ?(bps=default_bps) () = 
  let makemac node = ((new Mac_contention.contentionmac ~stack ~bps node) :> Mac.t) in
  install_macs_ ~stack makemac

let install_cheat_macs ?(stack=0) ?(bps=default_bps) () = 
  let makemac node = ((new Mac_cheat.cheatmac ~stack ~bps node) :> Mac.t) in
  install_macs_ ~stack makemac

let install_tdack_macs ?(stack=0) ?(bps=Narrowband_radios.xemics1205.Radiochips.rate) ?(chip=Narrowband_radios.xemics1205) () = 
  let makemac node = ((new Mac_tdack.tdackmac ~stack ~bps ~chip node) :> Mac.t) in
  install_macs_ ~stack makemac

let install_maca_simple_macs ?(stack=0) ?(bps=default_bps) () = 
  let makemac node = ((new MACA_simple.maca_mac ~stack ~bps node) :> Mac.t) in
  install_macs_ ~stack makemac

let install_maca_contention_macs ?(stack=0) ?(bps=default_bps) () = 
  let makemac node = ((new MACA_contention.maca_contentionmac ~stack ~bps node) :> Mac.t) in
  install_macs_ ~stack makemac

let install_macs ?(stack=0) ?(bps=default_bps) () = 
  match Mac.mac() with
    | Mac.Nullmac -> install_null_macs ~stack ~bps ()
    | Mac.QueueNullmac -> install_queue_null_macs ~stack ~bps ()
    | Mac.Contmac -> install_contention_macs ~stack ~bps ()
    | Mac.Cheatmac -> install_cheat_macs ~stack ~bps ()
    | Mac.MACA_simple -> install_maca_simple_macs ~stack ~bps ()
    | Mac.MACA_contention -> install_maca_contention_macs ~stack ~bps ()
    | Mac.Tdackmac -> install_tdack_macs ~stack ~bps ()


let install_mobs ?gran () = 
  match Param.get Mob_ctl.mob with 
    | `Uniwaypoint  -> Mob_ctl.make_uniwaypoint_mobs ?gran ()
    | `Borderwaypoint  -> Mob_ctl.make_borderwaypoint_mobs ?gran ()
    | `Epfl_waypoint  -> Mob_ctl.make_epfl_waypoint_mobs ()
    | `Billiard  -> Mob_ctl.make_billiard_mobs ?gran ()
    | `Randomwalk_1d  -> Mob_ctl.make_discrete_randomwalk_mobs ?gran ~dim:`One ()
    | `Randomwalk_2d  -> Mob_ctl.make_discrete_randomwalk_mobs ?gran ~dim:`Two ()
    | `None -> ()


let make_naked_nodes () = (
  Nodes.set_nodes [||]; (* in case this is being called a second time in the same script *)
  Nodes.set_nodes 
    (Array.init (Param.get Params.nodes)
      (fun i -> (new Node.node i)));

  (* set up initial node position in internal structures of World object *)
  Nodes.iteri (fun nid _ -> (World.w())#init_pos ~nid ~pos:(World.w())#random_pos);
  assert ((World.w())#neighbors_consistent);
)

let make_nodes () = (
  make_naked_nodes ();
  install_macs ();
)
    
let place_nodes_on_line () = 
  let x_incr = (Param.get Params.x_size) /.  float (Param.get Params.nodes) in
  Nodes.iteri (fun nid _ -> 
    let newpos = ((float nid) *. x_incr, 0.0) in
    (World.w())#movenode ~nid ~newpos)

let place_nodes_on_grid () = 

  let x_incr = (Param.get Params.x_size) /.  sqrt (float (Param.get Params.nodes)) in
  let y_incr = (Param.get Params.y_size) /.  sqrt (float (Param.get Params.nodes)) in
  let n_nodes_x = int (floor ((Param.get Params.x_size) /. x_incr)) in
  Nodes.iteri (fun nid _ -> 
    let newpos = ((float (nid mod n_nodes_x)) *. x_incr, (float (nid / n_nodes_x)) *. y_incr)  in
    (World.w())#movenode ~nid ~newpos)
  


let install_ler_agents ?(stack=0) proto = (
  Nodes.iteri (fun nid n -> 
    let agent = new Ler_agent.ler_agent ~stack ~proto n in
    n#install_rt_agent ~stack (agent :> Rt_agent.t));
)

let make_str_nodes metric = (
  make_nodes();

  Nodes.iteri (fun nid n -> 
    let agent = new Str_agent.str_agent ~stack:0 metric n in
    n#install_rt_agent ~stack:0 (agent :> Rt_agent.t));
)

let make_aodv_nodes ?(localrepair=true) ?(dstonly=false) () = (
  make_nodes();

  Nodes.iteri (fun nid n -> 
    let agent = Aodv_agent.make_aodv_agent ~stack:0 ~localrepair ~dstonly n in
    n#install_rt_agent ~stack:0 (agent :> Rt_agent.t));
)


let install_flood_agents ?(stack=0) () = (
  
  Nodes.iteri (fun nid n -> 
    let agent = (new Flood_agent.flood_agent ~stack n) in
    n#install_rt_agent ~stack (agent :> Rt_agent.t));
)


let avg_neighbors_per_node() = 
  let total_neighbors = 
    Nodes.fold (fun n total -> (List.length ((World.w())#neighbors n#id)) + total) 0 
  in
  (float total_neighbors) /. (float (Param.get Params.nodes))

let max_neighbors_per_node() = 
  let max = ref 0 in
  let _neighbors = 
    Nodes.iter (fun n -> 
      if List.length ((World.w())#neighbors n#id) > !max
      then max := List.length ((World.w())#neighbors n#id)  )
  in
  max

  
let print_header () = (
  Printf.printf "\n";
  Printf.printf "--------------------------------------- \n";
  Printf.printf "        nab - network in a box          \n";
  Printf.printf "--------------------------------------- \n\n";
  flush stdout;
)

 
let finish () = (
  Printf.printf ".. done\n";
  flush stdout;
  exit 0;
)


let detach_daemon ?outfilename () = (
  
  let fname =
    begin match outfilename with 
      | Some f -> f
      | None -> 
	  let status, name = 
	    command_output "date +%F-%Hh%Mm%S" in
	  if status <> 0 then 
	    failwith "Script_utils.detach: date command failed";
	  ("nab-"^(List.hd name)^".log")
    end 
  in 
  if Sys.file_exists fname then failwith ("Eeeeek!!! File "^fname^" already exists!!!");

  let pid =  Unix.fork () in
  if pid < 0 then failwith "Error in fork";
  if pid > 0 then exit 0;
  let _ = Unix.setsid () in
  close_in stdin;
  close_out stdout;
  close_out stderr;
  
  Log.ochan := open_out fname
    
)


(*
  density = area / nodes
  -> area = nodes * density (1)
  
  
  rsurface = 3.14 * (rrange^2)
  
  degree = density * rsurface
  -> density = rsurface/degree (2)
  
  
  (1) and (2) :
  area = nodes * rsurface / degree = nodes * (3.14 * (rrange^2)) / degree 
  
  side = sqrt(area)
*)
let size 
  ?(rrange=(Param.get Params.radiorange))
  ?(nodes=(Param.get Params.nodes)) ~avg_degree () = 
  
  let x = 
    match snd (Param.get World.world) with 
      | World.One -> (rrange *. (float nodes)) /. (float avg_degree)
      | World.Two ->  
	  sqrt((float nodes) *. (3.14 *. (rrange *. rrange)) /. (float avg_degree))
  and y = 
    match snd (Param.get World.world) with 
      | World.One -> 0.
      | World.Two ->  
	  sqrt((float nodes) *. (3.14 *. (rrange *. rrange)) /. (float avg_degree))
  in x, y

let interactive_print_banner s = 
  print_endline s

let abort_on_kill _ = 
  print_endline " \n ***\n *** Killed by SIGINT!\n ***\n";
  exit (-1)

let _ = 
  if !Sys.interactive then 
    print_endline ("\n     Network in a Box (nab) version "^Common.nab_release);
  Sys.set_signal Sys.sigint (Sys.Signal_handle abort_on_kill);
  
  Pervasives.at_exit (fun () -> Log.log#log_always 
    (lazy (Printf.sprintf "CPU Time: %f" (Sys.time()))))
    
