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
let nstacks = 2

(* Configure default parameters.
   For some parameters, like # nodes, these can also be provided on the
   command line, and will override the defaults given here during the call to 
   Script_utils.parse_args() below.
 *)
let () = 
  Param.set Params.nodes nodes; 
  Param.set Params.x_pix_size 900;
  Param.set Params.y_pix_size 900;

  (* Set the transmission range (range within which nodes are neighbors). *)
  Param.set Params.radiorange radiorange;

  (* Set the granularity at which nodes move. The smaller the granularity, the
     smaller the "steps" - this means also more events getting scheduled to move
     nodes around. *)
  Param.set Params.mob_gran (radiorange /. 2.);

  (* Set the number of targets in each nodes last-encounter table. *)
  Param.set Params.ntargets 1

let encounter_ratio = 
  (* We could simply define this as a float (as for radiorange above) - 
     but by creating a Param object it becomes possible to set this value via
     the command-line invocation. *)
  Param.floatcreate  ~name:"warmup"  ~cmdline:true  ~default:0.5
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
  Script_utils.init_greedy_world();
  
  (* Make "naked" (no mac, routing agents) nodes*)
  Script_utils.make_naked_nodes ~pos_aware:true ();

  (* Create macs on both stacks. *)
  Script_utils.install_macs ~stack:0 ();
  Script_utils.install_macs ~stack:1 ();


(* Create routing agents. On stack 0, they run GREASE, on stack 1 they run
   EASE. *)
  Script_utils.make_grease_agents ~ease:false ~stack:0 ();
  Script_utils.make_grease_agents ~ease:true ~stack:1 ();
  
  (* Create billiard mobility processes (see mob_ctl.mli, mobs.mli).*)
  Mob_ctl.make_billiard_mobs();

)



let do_one_route src = (

  let routeref = ref (Route.create()) in

  let in_mhook = Gui_hooks.ease_route_pktin_mhook routeref in
  let out_mhook = Gui_hooks.ease_route_pktout_mhook routeref in
  Nodes.gpsiter (fun n -> n#clear_pkt_mhooks ());
  Nodes.gpsiter (fun n -> n#add_pktin_mhook in_mhook);
  Nodes.gpsiter (fun n -> n#add_pktout_mhook out_mhook);

  (Nodes.gpsnode src)#originate_app_pkt ~dst:0;
  (Sched.s())#run();

  !routeref;
)

(* Move nodes until the required encounter ratio is reached *)
let warmup enc_ratio = (
  let finished = ref false in 
  
  while (not !finished) do 

    (* make nodes move for 60 seconds. *)
    (Sched.s())#run_for ~duration:60.0;
    
    let p = Script_utils.proportion_met_nodes() in
    Log.log#log_always (lazy (Printf.sprintf "Warming up: enc. ratio %f" p));

    if  (p > enc_ratio) then   finished := true;

  done;

  Log.log#log_always (lazy "Done warming up");
  Log.log#log_always (lazy (Printf.sprintf "Average density is %f."
    (Script_utils.avg_neighbors_per_node())));


)



let main() = 

  (* Parse command-line arguments, inform user *)
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
  
  (*
    Uncomment this if you want max debug output just during the route
    computation
    
    Log.set_log_level ~level:Log.LOG_DEBUG;
  *)
  repeat 0 (
    fun () -> 
      let src = (Random.int ((Param.get Params.nodes) - 1)) + 1
      in 
      Log.log#log_always (lazy (Printf.sprintf "Route from source %d" src));
      let route = do_one_route src   in ()
(*      Log.log#log_always (lazy (Route.sprintnid route));
      if Route.ease_route_valid route 
	~src
	~dst:0
      then print_endline "ok\n" 
*)
  )

let () = 
  main();
  Gui_gtk.init ();
  Gui_ease_diffusion.create_buttons_ease();
  Main.main()





