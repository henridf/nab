(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)



open Printf
open Misc
open Script_utils

let encounter_ratio = 0.5



(* This is called at the very beginning to configure default parameters.
   For some parameters, like # nodes, these can also be provided on the
   command line, and will override the defaults given here *)
let set_defaults() = (

  (* Set # nodes and area size. *)
  Param.set Params.nodes 400; 
  Param.set Params.x_size 20.0; 
  Param.set Params.y_size 20.0;

(* The geo-routing algorithm in the EASE agent used here sometimes needs to
   send a packet to a node which is not a physical neighbor (similar to what
   would happen with a delaunay triangulation), so we use the "cheatmac" -
   other MACs implemenations do not allow this. *)
  Param.set Params.mac "cheatmac";

  (* Set the transmission range (range within which nodes are neighbors). *)
  Param.set Params.rrange ( 1. +. epsilon_float);

  (* Set the number of targets in each nodes last-encounter table. *)
  Param.set Params.ntargets 1;
)


let setup () = (


  (* Instantiate global world object. This must be done *after* the
     params above have been set. *)
  Script_utils.init_greedy_world();
  
  (* Create nodes which are configured with GREASE routing protocol. *)
  Script_utils.make_grease_nodes();
  
  (* Create billiard mobility processes (see mob_ctl.mli, mobs.mli).*)
  Mob_ctl.make_billiard_mobs();

)



let do_one_route() = (

  let routeref = ref (Route.create()) in
  Gui_hooks.route_done := false;

  let in_mhook = Gui_hooks.ease_route_pktin_mhook routeref in
  let out_mhook = Gui_hooks.ease_route_pktout_mhook routeref in
  Nodes.gpsiter (fun n -> n#clear_pkt_mhooks);
  Nodes.gpsiter (fun n -> n#add_pktin_mhook in_mhook);
  Nodes.gpsiter (fun n -> n#add_pktout_mhook out_mhook);

  (Nodes.gpsnode 17)#originate_app_pkt ~dst:0;
  (Sched.s())#run();

  !routeref;
)

(* Move nodes until the required encounter ratio is reached *)
let warmup enc_ratio () = (
  let finished = ref false in 
  
  while (not !finished) do 

    (* make nodes move for 10 seconds. *)
    (Sched.s())#run_for ~duration:10.0;
    
    let p = Script_utils.proportion_met_nodes() in
    Log.log#log_always (lazy (Printf.sprintf "Warming up: enc. ratio %f" p));

    if  (p > enc_ratio) then   finished := true;

  done;

  Log.log#log_always (lazy "Done warming up");
  Log.log#log_always (lazy (Printf.sprintf "Average density is %f."
    (Script_utils.avg_neighbors_per_node())));


)



let main() = 

  (* Setup defaults. *)
  set_defaults();
  
  (* Parse command-line arguments, inform user *)
  Script_utils.parse_args();
  Script_utils.print_header();
  Param.printconfig stdout;

  (* Create nodes, GREASE agents, mob processes, etc *)
  setup();

  (* Start the mobility processes. *)
  Mob_ctl.start_all();

  (* Warmup until prescribed enc. ratio is attained. *)
  warmup encounter_ratio ();

  (* Stop the mobility processes. *)
  Mob_ctl.stop_all();
  
  (*
    Uncomment this if you want max debug output just during the route
    computation
    
    Log.set_log_level ~level:Log.LOG_DEBUG;
  *)
  let route = do_one_route()
  in
  
  if Route.ease_route_valid route 
    ~src:17
    ~dst:0
  then print_endline "ok\n" 


let () = main()




