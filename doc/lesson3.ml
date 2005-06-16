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

let nodes = 1000
let radiorange = 12.0
let avg_degree = 10
let nstacks = 3

(* Configure default parameters.
   For some parameters, like # nodes, these can also be provided on the
   command line, and will override the defaults given here during the call to 
   Script_utils.parse_args() below.
 *)
let () = 
  Param.set Params.nodes nodes; (* number of nodes *)

  Param.set Params.x_pix_size 600; (* size of gui window *)
  Param.set Params.y_pix_size 600;

  Param.set World.world World.Greedy;

  (* Set the transmission range (range within which nodes are neighbors). *)
  Param.set Params.radiorange radiorange;

  (* Set the number of targets in each nodes last-encounter table. *)
  Param.set Ler_agent.ntargets 1

    

let encounter_ratio = 
  (* We could simply define this as a float (as for radiorange above) - 
     but by creating a Param object it becomes possible to set this value via
     the command-line invocation. *)
  Param.floatcreate  ~name:"enc_ratio"  ~cmdline:true  ~default:0.5
    ~doc:"encounter ratio" ()
    

let set_values() = (
  (* Set the appropriate world size to have desired node degree, given the radio
     range *)
  let size = Script_utils.size ~rrange:radiorange  ~nodes:(Param.get Params.nodes) 
    ~avg_degree () in
  
  Param.set Params.x_size size;
  Param.set Params.y_size size;
  
  (* The geo-routing algorithm in the EASE agent used here sometimes needs to
     send a packet to a node which is not a physical neighbor (similar to what
     would happen with a delaunay triangulation), so we use the "cheatmac" -
     other MACs implemenations do not allow this. *)
  Param.set Params.mac "cheatmac";
)

let setup () = (

  (* Instantiate global world object. This must be done *after* the
     params above have been set. *)
  Script_utils.init_world();
  
  (* Make "naked" (no mac, routing agents) nodes*)
  Script_utils.make_naked_nodes ();

  (* Create macs on all stacks. *)
  for stack = 0 to nstacks - 1 do 
    Script_utils.install_macs ~stack ()
  done;

  (* Create GREASE, EASE, and FRESH routing agents on stacks 0, 1, and 2.*)
  Script_utils.install_ler_agents ~stack:0 Ler_agent.GREASE;
  Script_utils.install_ler_agents ~stack:1 Ler_agent.EASE;
  Script_utils.install_ler_agents ~stack:2 Ler_agent.FRESH;
  
  (* Create billiard mobility processes (see mob_ctl.mli, mobs.mli).*)
  Mob_ctl.make_billiard_mobs ~gran:(radiorange /. 2.) ();
)

(* Move nodes until the required encounter ratio is reached *)
let warmup enc_ratio = (
  let finished = ref false in 
  
  if enc_ratio > 0.0 then
    while (not !finished) do 
      
      (* make nodes move for 30 seconds. *)
      (Sched.s())#run_for ~duration:30.0;
      
      let p = Ler_utils.proportion_met_nodes() in
      Log.log#log_always (lazy (Printf.sprintf "Warming up: enc. ratio %f" p));
      
      if  (p > enc_ratio) then   finished := true;
      
    done;

  Log.log#log_always (lazy "Done warming up");
  Log.log#log_always (lazy (Printf.sprintf "Average density is %f."
    (Script_utils.avg_neighbors_per_node())));
)



let main() = (

  (* Parse command-line arguments. *)
  Script_utils.parse_args();
  set_values();
  Script_utils.print_header();
  Param.printconfig stdout;

  (* Create nodes, GREASE agents, mob processes, etc *)
  setup();

  (* Start the mobility processes. *)
  Mob_ctl.start_all();

  (* Warmup until prescribed enc. ratio is attained. *)
  warmup (Param.get encounter_ratio);

  (* Stop the mobility processes. *)
  Mob_ctl.stop_all();
)

(* This is simply there so that we can invoke this program with -script and
   have it exit immediately in automated tests. *)
let script = Param.boolcreate 
  ~name:"script" 
  ~doc:"exit immediately (for scripted tests)"
  ~cmdline:true
  ~default:false
  ~notpersist:true ()


let () = 
  main();
  Gui_gtk.init();
  Gui_ler.setup_easeviz_app();
  if not (Param.get script) then
    Main.main()
      
