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

(* 
   Simple functionality tests on a 3 node network.
   To run, simply do
   make SCRIPT=proto/aodv/scripts/test_3_nodes.ml nab   
   and run bin/nab

*)

open Test_utils

open Aodv_defaults
open Aodv_agent
open Aodv_stats


  
let install_aodv nid = 
  let n = Nodes.node nid in
  let agent = Aodv_agent.make_aodv_agent ~stack:0 n in
  n#install_rt_agent ~stack:0 (agent :> Rt_agent.t)

let _ = 
  Log.set_log_level Log.LOG_ALWAYS;

  testsuite 
    "Simple checks on a 3 node network";

  (* Setup world with 2 AODV nodes in range of each other. 
     We will setup the third node later.
  *)
  Param.set Params.nodes 3;
  Param.set Params.x_size 100.;
  Param.set Params.y_size 100.;
  Param.set Params.radiorange 21.;
  Script_utils.init_all();
  Script_utils.make_nodes();
  install_aodv 0;
  install_aodv 1;

  (World.w())#movenode ~nid:0 ~newpos:(0.0, 0.0);
  (World.w())#movenode ~nid:1 ~newpos:(20.0, 0.0);
  (World.w())#movenode ~nid:2 ~newpos:(40.0, 0.0);

  assert((World.w())#are_neighbors 0 1);
  assert((World.w())#are_neighbors 1 2);
  assert(not ((World.w())#are_neighbors 0 2))


let rt0 = (agent_rtab 0) and rt1 = (agent_rtab 1)
let sched = Sched.s() 

let _ =
  (* just in case there are any transient startup effects with timers, etc. *)
  sched#stop_in 10.;
  sched#run();

  msg 
    "Tests on a 2 node network: 0 -- 1";
  msg 
    "Sending a packet from node 1 to node 0";


  (Nodes.node 1)#originate_app_pkt ~l4pkt:`EMPTY ~dst:0;

  sched#run_until 
    ~continue:(fun () -> (agent_stats 0).Aodv_stats.data_recv <> 1);

    
  test 
    "Checking active route status"
    ((Aodv_rtab.have_active_route rt0)
    &&  
    (Aodv_rtab.have_active_route rt1));

  sched#stop_in (aodv_ACTIVE_ROUTE_TIMEOUT -. 0.1);
  sched#run();
  test 
    "Checking active route status (2)"
    ((Aodv_rtab.have_active_route rt0)
    &&  
    (Aodv_rtab.have_active_route rt1));
  
  sched#stop_in 0.2;
  sched#run();
  (* Now the active route on 1 should have timed out (not on node 0, because
     of the lifetime calculation in RFC 6.5. *)
  test 
    "Checking active route status (3)"
    ((Aodv_rtab.have_active_route rt0)
    &&
    (not (Aodv_rtab.have_active_route rt1)));
  
  test 
    "Checking packet stats"
    ((agent_stats 1).total_xmit = 2
    &&
    (agent_stats 0).total_xmit = 1);

  sched#stop_in 4.0;
  sched#run();

  test 
    "Checking active route status (4)"
    (not ((Aodv_rtab.have_active_route rt0)
    ||
    (Aodv_rtab.have_active_route rt1)));



  msg 
    "Tests on a 3 node network: 0 -- 1 -- 2";

  reset_stats 0;  
  reset_stats 1;  
  install_aodv 2;
  
  msg 
    "Sending a packet from node 1 to node 0";

  (Nodes.node 1)#originate_app_pkt ~l4pkt:`EMPTY ~dst:0;

  sched#run_until 
    ~continue:(fun () -> (agent_stats 0).Aodv_stats.data_recv <> 1);

  msg 
    "Sending a packet from node 2 to node 1";
  reset_stats 0;  
  reset_stats 1;  
  reset_stats 2;  

  (Nodes.node 2)#originate_app_pkt ~l4pkt:`EMPTY ~dst:1;

  sched#run_until 
    ~continue:(fun () -> (agent_stats 1).Aodv_stats.data_recv <> 1);

  test 
    (* since 2 has overhead a broadcast from 1, it has a route and should not
       need to issue a rreq to send the packet to 1. *)
    "Checking packet stats"
    ((agent_stats 2).rreq_xmit = 0
      && (total_stats()).rrep_xmit = 0
    );

  msg 
    "Sending a packet from node 2 to node 0";
  reset_stats 0;  
  reset_stats 1;  
  reset_stats 2;  

  (Nodes.node 2)#originate_app_pkt ~l4pkt:`EMPTY ~dst:0;


  sched#run_until 
    ~continue:(fun () -> (agent_stats 0).Aodv_stats.data_recv <> 1);
  test 
    (* Here, we want to make sure intermediate route reply is working - and so
       that node 1 answers the rreq, not node 0.
       In fact node 1 sends a rrep to 2, and a GRAT rrep to 0 - so has
       transmitted 2 rreps. *)
    "Checking packet stats"
    ((agent_stats 0).rrep_xmit = 0
      &&
      (agent_stats 1).rrep_xmit = 2);


(*
  sched#sched_in 
    ~f:(fun () -> (Nodes.node 0)#originate_app_pkt ~l4pkt:`EMPTY ~dst:1)
    ~t:0.1;
  sched#sched_in 
    ~f:(fun () -> (Nodes.node 0)#originate_app_pkt ~l4pkt:`EMPTY ~dst:1)
    ~t:2.1;
  (Nodes.node 0)#originate_app_pkt ~l4pkt:`EMPTY ~dst:1;
  sched#stop_in (aodv_ACTIVE_ROUTE_TIMEOUT -. 0.1);
  sched#run();
  test 
    "Checking active route status (2)"
    ((Aodv_rtab.have_active_route rt0)
    &&  
    (Aodv_rtab.have_active_route rt1));
*)

testsuite_finished()
