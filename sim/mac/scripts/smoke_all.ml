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
   Simple smoke test on a two node network.
   To run, simply do
   make SCRIPT=sim/mac/scripts/smoke_all.ml nab   
   and run bin/nab
*)


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

  
let _ = 
  Log.set_log_level Log.LOG_DEBUG;

  testsuite 
    "Connectivity tests for null mac";

  (* Setup world with 2 nodes in range of each other.   *)

  Param.set Params.nodes 2;
  Param.set Params.x_size 2.;
  Param.set Params.y_size 2.;
  Param.set Params.radiorange 2.;
  Param.set Params.mac "nullmac";
  Script_utils.init_all();
  Script_utils.make_nodes();
  (World.w())#movenode ~nid:0 ~newpos:(0.0, 0.0);
  (World.w())#movenode ~nid:1 ~newpos:(1.0, 1.0)

   

let sched = Sched.s() 
let mac0 = Mac_base.mac 0 and mac1 = Mac_base.mac 1

let make_pkt src dst ?ttl () = 
  let l4pkt = `EMPTY in
  let l3hdr = L3pkt.make_l3hdr ~src ~dst ?ttl () in
  let l3pkt = L3pkt.make_l3pkt ~l3hdr ~l4pkt in
  let l2pkt = L2pkt.make_l2pkt ~src ~dst l3pkt in
  l2pkt


let _ =
  (* just in case there are any transient startup effects with timers, etc. *)
  sched#stop_in 10.;
  sched#run();

  msg 
    "Tests on a 2 node network: 0 -- 1";
  msg 
    "Unicast packets";

  mac0#xmit (make_pkt 0 1 ());

  sched#run_for ~duration:5.;
    
  test 
    "Checking packet counters"
    true;


  exit 0
