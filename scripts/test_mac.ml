(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Header *)







open Printf
open Misc
open Script_utils
open Experiment

let rrange = 100. 
  
let run_and_print_stats() = 
  (Sched.s())#run();

  Printf.printf "Sent %d , recv %d , orig %d\n"
  !Grep_hooks.data_pkts_sent !Grep_hooks.data_pkts_recv !Grep_hooks.data_pkts_orig

let test_2_nodes ()= 

  Param.set Params.rrange rrange;
  Param.set Params.mac "cont";
  Param.set Params.nodes 2;

  let pkts_to_send = 100 in
  let node_spacing = rrange *. 0.7 in
  Param.set Params.x_size (node_spacing *. float (Param.get Params.nodes));
  Param.set Params.y_size  rrange;
  
  init_sched();
  init_lazy_world();
  
  make_grep_nodes();

  let y_pos = (Param.get (Params.y_size)) /. 2.0 in
  Nodes.iteri (fun nid _ -> 
    let newpos = ( (float nid) *. node_spacing, y_pos ) in
    (World.w())#movenode ~nid ~newpos 
  );
  
  Grep_hooks.set_sources 1;
  Grep_hooks.set_stop_thresh (pkts_to_send + 1);
  
  let pkt_reception () = 
    (Nodes.node 0)#set_trafficsource 
    ~gen:(Tsource.make_cbr 
      ~num_pkts:pkts_to_send 
      ~pkts_per_sec:100.)
      ~dst:((Param.get Params.nodes) - 1);
  in
  (Sched.s())#sched_at ~f:pkt_reception ~t:Sched.ASAP;
  run_and_print_stats()  


let test_long_string ()= 


  Param.set Params.rrange rrange;
  Param.set Params.nodes 10;

  let pkts_to_send = 100 in
  let node_spacing = rrange *. 0.7 in
  Param.set Params.x_size (node_spacing *. float (Param.get Params.nodes));
  Param.set Params.y_size  rrange;
  
  init_sched();
  init_lazy_world();
  
  make_grep_nodes();

  let y_pos = (Param.get (Params.y_size)) /. 2.0 in
  Nodes.iteri (fun nid _ -> 
    let newpos = ( (float nid) *. node_spacing, y_pos ) in
    (World.w())#movenode ~nid ~newpos 
  );
  
  Grep_hooks.set_sources 1;
  Grep_hooks.set_stop_thresh (pkts_to_send + 1);
  
  let pkt_reception () = 
    (Nodes.node 0)#set_trafficsource 
    ~gen:(Tsource.make_cbr 
      ~num_pkts:pkts_to_send 
      ~pkts_per_sec:1.)
      ~dst:((Param.get Params.nodes) - 1);
  in
  (Sched.s())#sched_at ~f:pkt_reception ~t:Sched.ASAP;
  
  run_and_print_stats()


let test_3_nodes() = 
  Param.set Params.rrange rrange;
  Param.set Params.nodes 3;

  let pkts_to_send = 100 in
  Param.set Params.x_size 300.;
  Param.set Params.y_size 300.;
  
  init_sched();
  init_lazy_world();
  
  make_grep_nodes();

  (World.w())#movenode ~nid:0 ~newpos:(0.0, 0.0); 
  (World.w())#movenode ~nid:1 ~newpos:(50.0, 0.0); 
  (World.w())#movenode ~nid:2 ~newpos:(25.0, 25.0); 
  
  Grep_hooks.set_sources 1;
  Grep_hooks.set_stop_thresh (pkts_to_send + 1);
  
  let start_0 () = 
    (Nodes.node 0)#set_trafficsource 
    ~gen:(Tsource.make_cbr
      ~num_pkts:pkts_to_send 
      ~pkts_per_sec:1.) 
      ~dst:1
  and start_2 () = 
    (Nodes.node 2)#set_trafficsource 
      ~gen:(Tsource.make_cbr
	~num_pkts:pkts_to_send 
      ~pkts_per_sec:1.) 
      ~dst:1
    in
    
  (Sched.s())#sched_at ~f:start_0 ~t:Sched.ASAP;
  (Sched.s())#sched_in ~f:start_2 ~t:5.;
  
  run_and_print_stats()



let _ = 
  Log.set_log_level ~level:Log.LOG_DEBUG;
  test_3_nodes()
