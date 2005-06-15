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
   Simple functionality tests on a 7 node network.
   To run, simply do
   make SCRIPT=proto/aodv/scripts/test_7_nodes.ml nab   
   and run bin/nab
*)

open Aodv_defaults
open Aodv_agent
open Aodv_stats
open Test_utils


let sortList = List.sort compare
  
let _ = 
  Log.set_log_level Log.LOG_ALWAYS;

  testsuite 
    "Simple checks on a 7 node network";
  msg "Topology is:    ";
  msg " 0                  5";
  msg "   \\               / ";
  msg "      2 -- 3 -- 4    ";
  msg "   /               \\ ";
  msg " 1                  6";



  (* Setup world with 2 AODV nodes in range of each other. 
     We will setup the third node later.
  *)
  Param.set Params.nodes 7;
  Param.set Params.x_size 100.;
  Param.set Params.y_size 100.;
  Param.set Params.radiorange 21.;
  Script_utils.init_all();
  Script_utils.make_aodv_nodes ~localrepair:false ();

  (World.w())#movenode ~nid:0 ~newpos:(0.0, 22.0);
  (World.w())#movenode ~nid:1 ~newpos:(0.0, 0.0);
  (World.w())#movenode ~nid:2 ~newpos:(10.0, 10.0);
  (World.w())#movenode ~nid:3 ~newpos:(25.0, 10.0);
  (World.w())#movenode ~nid:4 ~newpos:(40.0, 10.0);
  (World.w())#movenode ~nid:5 ~newpos:(50.0, 22.0);
  (World.w())#movenode ~nid:6 ~newpos:(50.0, 0.0);

  assert(((World.w())#neighbors 0) = [2]);
  assert(((World.w())#neighbors 1) = [2]);
  assert(sortList ((World.w())#neighbors 2) = [0; 1; 3]);
  assert(sortList ((World.w())#neighbors 3) = [2; 4]);
  assert(sortList ((World.w())#neighbors 4) = [3; 5; 6]);
  assert(((World.w())#neighbors 5) = [4]);
  assert(((World.w())#neighbors 6) = [4])



let rt0 = (agent_rtab 0) 
and rt1 = (agent_rtab 1)
and rt2 = (agent_rtab 2)
and rt3 = (agent_rtab 3)
and rt4 = (agent_rtab 4)
and rt5 = (agent_rtab 5)
and rt6 = (agent_rtab 6)

let sched = Sched.s() 

let _ =
  (* just in case there are any transient startup effects with timers, etc. *)
  sched#stop_in 10.;
  sched#run();

  msg 
    "Sending a packet from node 5 to node 1";
  msg 
    "Sending a packet from node 6 to node 2";


  (Nodes.node 5)#originate_app_pkt ~l4pkt:`EMPTY ~dst:1;
  (Nodes.node 6)#originate_app_pkt ~l4pkt:`EMPTY ~dst:2;

  sched#run_until 
    ~continue:(fun () -> (total_stats()).Aodv_stats.data_recv <> 2);

  test 
    "Checking precursor lists node 0"
    (
      (Aodv_rtab.precursors rt0 0 = []) &&
      (Aodv_rtab.precursors rt0 1 = []) &&    
      (Aodv_rtab.precursors rt0 2 = []) &&    
      (Aodv_rtab.precursors rt0 3 = []) &&    
      (Aodv_rtab.precursors rt0 4 = []) &&    
      (Aodv_rtab.precursors rt0 5 = []) &&    
      (Aodv_rtab.precursors rt0 6 = []) &&
      not (Aodv_rtab.have_many_precursors rt0 [0; 1; 2; 4; 5; 6]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt0 2)) = [2; 5] 
    );

  test 
    "Checking precursor lists node 1"
    (
      (Aodv_rtab.precursors rt1 0 = []) &&
      (Aodv_rtab.precursors rt1 1 = []) &&    
      (Aodv_rtab.precursors rt1 2 = []) &&    
      (Aodv_rtab.precursors rt1 4 = []) &&    
      (Aodv_rtab.precursors rt1 5 = []) &&    
      (Aodv_rtab.precursors rt1 6 = []) &&
      not (Aodv_rtab.have_many_precursors rt1 [0; 1; 2; 4; 5; 6]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt1 2)) = [2; 5] 
    );

  test 
    "Checking precursor lists node 2"
    (
      (Aodv_rtab.precursors rt2 0 = []) &&
      (Aodv_rtab.precursors rt2 1 = [3]) &&    
      (Aodv_rtab.precursors rt2 2 = []) &&    
      (Aodv_rtab.precursors rt2 4 = []) &&    
      (Aodv_rtab.precursors rt2 5 = []) &&    
      (Aodv_rtab.precursors rt2 6 = []) &&
      not (Aodv_rtab.have_many_precursors rt2 [0; 1; 2; 4; 5; 6]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt2 3)) = [3; 5; 6]
    );


  test 
    "Checking precursor lists node 3"
    (
      (Aodv_rtab.precursors rt3 0 = []) &&
      (Aodv_rtab.precursors rt3 1 = [4]) &&    
      (Aodv_rtab.precursors rt3 2 = [4]) &&    
      (Aodv_rtab.precursors rt3 4 = []) &&    
      (Aodv_rtab.precursors rt3 5 = []) &&    
      (Aodv_rtab.precursors rt3 6 = []) &&
      not (Aodv_rtab.have_many_precursors rt3 [0; 1; 2; 4; 5; 6]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt3 2)) = [1; 2]
    );

  test 
    "Checking precursor lists node 4"
    (
      (Aodv_rtab.precursors rt4 0 = []) &&
      (Aodv_rtab.precursors rt4 1 = [5]) &&    
      (Aodv_rtab.precursors rt4 2 = [6]) &&    
      (Aodv_rtab.precursors rt4 4 = []) &&    
      (Aodv_rtab.precursors rt4 5 = []) &&    
      (Aodv_rtab.precursors rt4 6 = []) &&
      (Aodv_rtab.have_many_precursors rt4 [1; 2]) &&
      not (Aodv_rtab.have_many_precursors rt4 [0; 5; 6]) &&
      not (Aodv_rtab.have_many_precursors rt4 [1]) &&
      not (Aodv_rtab.have_many_precursors rt4 [2]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt4 3)) = [1; 2; 3]
    );

  test 
    "Checking precursor lists node 5"
    (
      (Aodv_rtab.precursors rt5 0 = []) &&
      (Aodv_rtab.precursors rt5 1 = []) &&    
      (Aodv_rtab.precursors rt5 2 = []) &&    
      (Aodv_rtab.precursors rt5 4 = []) &&    
      (Aodv_rtab.precursors rt5 5 = []) &&    
      (Aodv_rtab.precursors rt5 6 = []) &&
      not (Aodv_rtab.have_many_precursors rt5 [0; 1; 2; 4; 5; 6]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt5 4)) = [1; 4; 6]
    );

  test 
    "Checking precursor lists node 6"
    (
      (Aodv_rtab.precursors rt6 0 = []) &&
      (Aodv_rtab.precursors rt6 1 = []) &&    
      (Aodv_rtab.precursors rt6 2 = []) &&    
      (Aodv_rtab.precursors rt6 4 = []) &&    
      (Aodv_rtab.precursors rt6 5 = []) &&    
      (Aodv_rtab.precursors rt6 6 = []) &&
      not (Aodv_rtab.have_many_precursors rt6 [0; 1; 2; 4; 5; 6]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt6 4)) = [2; 4; 5]
    );

  let nexthops = [
    (rt0, 1, None); 
    (rt0, 2, Some 2);  
    (rt0, 3, None);  
    (rt0, 4, None);
    (rt0, 5, Some 2); 
    (rt0, 6, None);
    (rt1, 0, None);
    (rt1, 2, Some 2);
    (rt1, 3, None);
    (rt1, 4, None);
    (rt1, 5, Some 2);
    (rt1, 6, None);
    (rt2, 0, Some 0);
    (rt2, 1, Some 1);
    (rt2, 3, Some 3);
    (rt2, 4, None);
    (rt2, 5, Some 3);
    (rt2, 6, Some 3);
    (rt4, 0, None);
    (rt4, 1, Some 3);
    (rt4, 2, Some 3);
    (rt4, 3, Some 3);
    (rt4, 5, Some 5);
    (rt4, 6, Some 6);
    (rt5, 0, None);
    (rt5, 1, Some 4);
    (rt5, 2, None);
    (rt5, 3, None);
    (rt5, 4, Some 4);
    (rt5, 6, Some 4);
    (rt6, 0, None);
    (rt6, 1, None);
    (rt6, 2, Some 4);
    (rt6, 3, None);
    (rt6, 4, Some 4);
    (rt6, 5, Some 4);
  ] in


  test "Checking next-hop entries on all nodes"
    (List.fold_left (fun bool (rt, dst, nh) -> 
      Aodv_rtab.nexthop_opt rt dst = nh && bool) true nexthops);

  msg "moving node 2 out of range";
  (World.w())#movenode ~nid:2 ~newpos:(100.0, 100.0);

  for i = 0 to 6 do reset_stats i done;

  msg "sending a packet from node 4 to node 2"; 
  (Nodes.node 4)#originate_app_pkt ~l4pkt:`EMPTY ~dst:1; 

  sched#run_for ~duration:1.;

  test "checking nodes upstream of 2 receive proper RERRs"
    (
      (Aodv_rtab.nexthop_opt rt3 1 = None) &&
      (Aodv_rtab.nexthop_opt rt3 2 = None) &&
      (Aodv_rtab.nexthop_opt rt4 1 = None) &&
      (Aodv_rtab.nexthop_opt rt4 2 = None) &&
      (Aodv_rtab.nexthop_opt rt5 1 = None) &&
      (Aodv_rtab.nexthop_opt rt5 2 = None) &&
      (Aodv_rtab.nexthop_opt rt6 1 = None) &&
      (Aodv_rtab.nexthop_opt rt6 2 = None)
    );

testsuite_finished()
