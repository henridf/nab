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
   Simple functionality tests on a 6 node network.
   To run, simply do
   make SCRIPT=proto/aodv/scripts/test_6_nodes.ml nab   
   and run bin/nab

*)

open Aodv_defaults
open Aodv_agent
open Aodv_stats

let testno = ref 1
let failures = ref 0
  
let testsuite msg = 
  Printf.printf "\n\n";
  Printf.printf
    "--------------------------------------------------------------------\n";
  Printf.printf "Testsuite: %s\n\n" msg;
  flush stdout


let test msg pass = 
  Printf.printf "Testcase %d: %s... " !testno msg;
  flush stdout;
  incr testno;
  if pass then Printf.printf "Succeeded!\n"
  else 
    (Printf.printf "*** FAILED.\n"; incr failures);
  flush stdout
    

let msg m = print_endline ("[ "^m^" ]"); flush stdout

let sortList = List.sort compare
  
let install_aodv nid = 
  let n = Nodes.node nid in
  let agent = Aodv_agent.make_aodv_agent ~stack:0 n in
  n#install_rt_agent ~stack:0 (agent :> Rt_agent.t)

let _ = 
  Log.set_log_level Log.LOG_ALWAYS;

  testsuite 
    "Simple checks on a 6 node network";
  msg "Topology is:    ";
  msg " 0             4";
  msg "   \\          / ";
  msg "      2 -- 3    ";
  msg "   /          \\ ";
  msg " 1             5";



  (* Setup world with 2 AODV nodes in range of each other. 
     We will setup the third node later.
  *)
  Param.set Params.nodes 6;
  Param.set Params.x_size 100.;
  Param.set Params.y_size 100.;
  Param.set Params.radiorange 21.;
  Script_utils.init_all();
  Script_utils.make_aodv_nodes();

  (World.w())#movenode ~nid:0 ~newpos:(0.0, 22.0);
  (World.w())#movenode ~nid:1 ~newpos:(0.0, 0.0);
  (World.w())#movenode ~nid:2 ~newpos:(10.0, 10.0);
  (World.w())#movenode ~nid:3 ~newpos:(30.0, 10.0);
  (World.w())#movenode ~nid:4 ~newpos:(40.0, 22.0);
  (World.w())#movenode ~nid:5 ~newpos:(40.0, 0.0);

  assert(((World.w())#neighbors 0) = [2]);
  assert(((World.w())#neighbors 1) = [2]);
  assert(sortList ((World.w())#neighbors 2) = [0; 1; 3]);
  assert(sortList ((World.w())#neighbors 3) = [2; 4; 5]);
  assert(((World.w())#neighbors 4) = [3]);
  assert(((World.w())#neighbors 5) = [3])



let rt0 = (agent_rtab 0) 
and rt1 = (agent_rtab 1)
and rt2 = (agent_rtab 2)
and rt3 = (agent_rtab 3)
and rt4 = (agent_rtab 4)
and rt5 = (agent_rtab 5)

let sched = Sched.s() 

let _ =
  (* just in case there are any transient startup effects with timers, etc. *)
  sched#stop_in 10.;
  sched#run();

  msg 
    "Sending a packet from node 4 to node 1";
  msg 
    "Sending a packet from node 5 to node 2";


  (Nodes.node 4)#originate_app_pkt ~l4pkt:`EMPTY ~dst:1;
  (Nodes.node 5)#originate_app_pkt ~l4pkt:`EMPTY ~dst:2;

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
      not (Aodv_rtab.have_many_precursors rt0 [0; 1; 2; 3; 4; 5]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt0 2)) = [2; 4] 
    );

  test 
    "Checking precursor lists node 1"
    (
      (Aodv_rtab.precursors rt1 0 = []) &&
      (Aodv_rtab.precursors rt1 1 = []) &&    
      (Aodv_rtab.precursors rt1 2 = []) &&    
      (Aodv_rtab.precursors rt1 3 = []) &&    
      (Aodv_rtab.precursors rt1 4 = []) &&    
      (Aodv_rtab.precursors rt1 5 = []) &&
      not (Aodv_rtab.have_many_precursors rt1 [0; 1; 2; 3; 4; 5]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt1 2)) = [2; 4] 
    );

  test 
    "Checking precursor lists node 2"
    (
      (Aodv_rtab.precursors rt2 0 = []) &&
      (Aodv_rtab.precursors rt2 1 = [3]) &&    
      (Aodv_rtab.precursors rt2 2 = []) &&    
      (Aodv_rtab.precursors rt2 3 = []) &&    
      (Aodv_rtab.precursors rt2 4 = []) &&    
      (Aodv_rtab.precursors rt2 5 = []) &&
      not (Aodv_rtab.have_many_precursors rt2 [0; 1; 2; 3; 4; 5]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt2 3)) = [3; 4; 5]
    );

  test 
    "Checking precursor lists node 3"
    (
      (Aodv_rtab.precursors rt3 0 = []) &&
      (Aodv_rtab.precursors rt3 1 = [4]) &&    
      (Aodv_rtab.precursors rt3 2 = [5]) &&    
      (Aodv_rtab.precursors rt3 3 = []) &&    
      (Aodv_rtab.precursors rt3 4 = []) &&    
      (Aodv_rtab.precursors rt3 5 = []) &&
      (Aodv_rtab.have_many_precursors rt3 [1; 2]) &&
      not (Aodv_rtab.have_many_precursors rt3 [0; 4; 5]) &&
      not (Aodv_rtab.have_many_precursors rt3 [1]) &&
      not (Aodv_rtab.have_many_precursors rt3 [2]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt3 2)) = [1; 2]
    );

  test 
    "Checking precursor lists node 4"
    (
      (Aodv_rtab.precursors rt4 0 = []) &&
      (Aodv_rtab.precursors rt4 1 = []) &&    
      (Aodv_rtab.precursors rt4 2 = []) &&    
      (Aodv_rtab.precursors rt4 3 = []) &&    
      (Aodv_rtab.precursors rt4 4 = []) &&    
      (Aodv_rtab.precursors rt4 5 = []) &&
      not (Aodv_rtab.have_many_precursors rt4 [0; 1; 2; 3; 4; 5]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt4 3)) = [1; 3; 5]
    );

  test 
    "Checking precursor lists node 5"
    (
      (Aodv_rtab.precursors rt5 0 = []) &&
      (Aodv_rtab.precursors rt5 1 = []) &&    
      (Aodv_rtab.precursors rt5 2 = []) &&    
      (Aodv_rtab.precursors rt5 3 = []) &&    
      (Aodv_rtab.precursors rt5 4 = []) &&    
      (Aodv_rtab.precursors rt5 5 = []) &&
      not (Aodv_rtab.have_many_precursors rt5 [0; 1; 2; 3; 4; 5]) &&
      (sortList (Aodv_rtab.dests_thru_hop rt5 3)) = [2; 3; 4]
    );

  let nexthops = [
    (rt0, 1, None); 
    (rt0, 2, Some 2);  
    (rt0, 3, None);
    (rt0, 4, Some 2); 
    (rt0, 5, None);
    (rt1, 0, None);
    (rt1, 2, Some 2);
    (rt1, 3, None);
    (rt1, 4, Some 2);
    (rt1, 5, None);
    (rt2, 0, None);
    (rt2, 1, Some 1);
    (rt2, 3, Some 3);
    (rt2, 4, Some 3);
    (rt2, 5, Some 3);
    (rt3, 0, None);
    (rt3, 1, Some 2);
    (rt3, 2, Some 2);
    (rt3, 4, Some 4);
    (rt3, 5, Some 5);
    (rt4, 0, None);
    (rt4, 1, Some 3);
    (rt4, 2, None);
    (rt4, 3, Some 3);
    (rt4, 5, Some 3);
    (rt5, 0, None);
    (rt5, 1, None);
    (rt5, 2, Some 3);
    (rt5, 3, Some 3);
    (rt5, 4, Some 3);
  ] in


  test "Checking next-hop entries on all nodes"
    (List.fold_left (fun bool (rt, dst, nh) -> 
      Aodv_rtab.nexthop_opt rt dst = nh && bool) true nexthops);
      
  msg "moving node 2 out of range";
  (World.w())#movenode ~nid:2 ~newpos:(100.0, 100.0);

  for i = 0 to 5 do reset_stats i done;

  msg "sending a packet from node 3 to node 2"; 
  (Nodes.node 3)#originate_app_pkt ~l4pkt:`EMPTY ~dst:2; 

  sched#run_for ~duration:40.;
  test "Checking that Node 3 abandoned after 5 retries (ttl 3, 5, 7, 35, 35)"
    (
      ((agent_stats 3).rreq_init = 1)
      &&
      ((agent_stats 3).rreq_orig = 5)
    );

  test "Checking that no RERRs were sent"
    (
      ((total_stats()).rerr_orig = 0) &&
      ((total_stats()).rerr_xmit = 0)
    )
