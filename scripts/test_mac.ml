(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils
open Experiment

let avg_degree = 12
let rrange = 100. 





let test_long_string ()= 
  Log.set_log_level ~level:Log.LOG_NOTICE;

  Param.set Params.rrange rrange;
  Param.set Params.nodes 10;

  let pkts_to_send = 100 in
  let node_spacing = rrange *. 0.7 in
  Param.set Params.x_size (node_spacing *. float (Param.get Params.nodes));
  Param.set Params.y_size  rrange;
  
  init_sched();
  init_world();
  
  make_grep_nodes();

  let y_pos = (Param.get (Params.y_size)) /. 2.0 in
  Nodes.iteri (fun nid -> 
    let newpos = ( (float nid) *. node_spacing, y_pos ) in
    (Gworld.world())#movenode ~nid ~newpos 
  );
  
  Grep_hooks.set_sources 1;
  Grep_hooks.set_stop_thresh (pkts_to_send + 1);
  
  let pkt_reception () = 
    (Nodes.node 0)#trafficsource 
    ~num_pkts:pkts_to_send 
      ~dst:((Param.get Params.nodes) - 1)
      ~pkts_per_sec:1;
  in
  (Gsched.sched())#sched_at ~f:pkt_reception ~t:Sched.ASAP;
  
  let start_time = Common.get_time() in
  (Gsched.sched())#run();

  Printf.printf "Sent %d , recv %d , orig %d\n"
  !Grep_hooks.data_pkts_sent !Grep_hooks.data_pkts_recv !Grep_hooks.data_pkts_orig

  
  

let test_3_nodes() = 
  Log.set_log_level ~level:Log.LOG_DEBUG;
  Param.set Params.rrange rrange;
  Param.set Params.nodes 3;

  let pkts_to_send = 100 in
  Param.set Params.x_size 300.;
  Param.set Params.y_size 300.;
  
  init_sched();
  init_world();
  
  make_grep_nodes();

  (Gworld.world())#movenode ~nid:0 ~newpos:(0.0, 0.0); 
  (Gworld.world())#movenode ~nid:1 ~newpos:(50.0, 0.0); 
  (Gworld.world())#movenode ~nid:2 ~newpos:(25.0, 25.0); 
  
  Grep_hooks.set_sources 1;
  Grep_hooks.set_stop_thresh (pkts_to_send + 1);
  
  let pkt_reception () = 
    (Nodes.node 0)#trafficsource 
    ~num_pkts:pkts_to_send 
      ~dst:1
      ~pkts_per_sec:1 
  in

  (Gsched.sched())#sched_at ~f:pkt_reception ~t:Sched.ASAP;
  
  let start_time = Common.get_time() in
  (Gsched.sched())#run();
  
  Printf.printf "Sent %d , recv %d , orig %d, total sent %d\n"
    !Grep_hooks.data_pkts_sent !Grep_hooks.data_pkts_recv
    !Grep_hooks.data_pkts_orig !Grep_hooks.total_pkts_sent


let _ = test_long_string()
