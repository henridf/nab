(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils

type agent_type = AODV | GREP
let agent_string t = (
  match t with | AODV -> "AODV" | GREP -> "GREP"
)
let run = ref 0

let node_degree = 12
let rrange = 100.
let size nodes = 
  sqrt( (float nodes) *. (3.14 *. (rrange *. rrange)) /. 
    float (node_degree))

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


let res_summary = ref []

let do_one_run ~agenttype ~nodes ~sources ~packet_rate ~speed
  ~pkts_to_recv = (
  Random.init 124231;
  incr run;

  Log.set_log_level ~level:Log.LOG_NOTICE;
  Param.set Params.nodes nodes;
  Param.set Params.rrange rrange;
  Param.set Params.x_size (size nodes);
  Param.set Params.y_size (size nodes);
  
  init_sched();
  init_world();
  
  begin match agenttype with
    | AODV -> make_aodv_nodes()
    | GREP -> make_grep_nodes();
  end;

  (* Attach a random waypoint mobility process to each node *)
  Mob_ctl.make_waypoint_mobs();
  Mob_ctl.set_speed_mps speed;
  Mob_ctl.start_all();


  Grep_hooks.set_sources sources;
  Grep_hooks.set_stop_thresh pkts_to_recv;


  Nodes.iter (fun n ->
    if (n#id < sources) then (
      let pkt_reception() = n#trafficsource (n#id + 1) packet_rate in
      (Gsched.sched())#sched_at ~f:pkt_reception ~t:(Sched.ASAP);
    )
  );
  
  Printf.fprintf stderr "\n";
  Printf.fprintf stderr "---------------------------\n";
  Printf.fprintf stderr "\n";
  Printf.fprintf stderr "Run %d parameters:\n" !run;
  Printf.fprintf stderr "%d Nodes, type %s\t\t\n" nodes (agent_string agenttype);
  Printf.fprintf stderr "Sources: %d\t\t\n" sources;
  Printf.fprintf stderr "%d [pkt/s], %f [m/s] \n" packet_rate speed ;
  Printf.fprintf stderr "%d [pkt bufsize]\n" Grep_agent.packet_buffer_size;
  let start_time = Common.get_time() in
(*  (Gsched.sched())#stop_in 40.0;*)
  (Gsched.sched())#run();
  

  Printf.fprintf stderr "\n";
  Printf.fprintf stderr "Results:\n" ;
  Printf.fprintf stderr "Packets sent: \t%d\n" !Grep_hooks.total_pkts_sent;
  Printf.fprintf stderr "DATA sent: \t%d\n" !Grep_hooks.data_pkts_sent;
  Printf.fprintf stderr "DATA dropped: \t%d\n" !Grep_hooks.data_pkts_drop;
  Printf.fprintf stderr "DATA dropped on rerr: \t%d\n" !Grep_hooks.data_pkts_drop_rerr;
  Printf.fprintf stderr "DATA orig: \t%d\n" !Grep_hooks.data_pkts_orig;
  Printf.fprintf stderr "DATA recv: \t%d\n" !Grep_hooks.data_pkts_recv;
  Printf.fprintf stderr "RREQ send: \t%d\n" !Grep_hooks.rreq_pkts_sent;
  Printf.fprintf stderr "RREP/RERR send: \t%d\n" !Grep_hooks.rrep_rerr_pkts_sent;
  let avgn = avg_neighbors_per_node() in
  let end_time = Common.get_time() in
  Printf.fprintf stderr "Time elapsed is %f\n" (end_time -. start_time);
  Printf.fprintf stderr "Avg neighbors per node is %f\n" avgn;

  res_summary := (speed, !Grep_hooks.total_pkts_sent)::!res_summary  ;
  
  flush stderr
)



let runs = [
(* speed routing rate nodes srcs pktsrecv *)
  (* 400 nodes *)
  (0.0, GREP, 1, 800, 1, 1000);
  (0.0, AODV, 1, 800, 1, 1000);
  (1.0, GREP, 1, 800, 1, 1000);
  (1.0, AODV, 1, 800, 1, 1000);
  (2.0, GREP, 1, 800, 1, 1000);
  (2.0, AODV, 1, 800, 1, 1000);
  (4.0, GREP, 1, 800, 1, 1000);
  (4.0, AODV, 1, 800, 1, 1000);
  (8.0, GREP, 1, 800, 1, 1000);
  (8.0, AODV, 1, 800, 1, 1000);
]



(*
let runs = [
(* speed routing rate nodes srcs pktsrecv *)
  (2.0, GREP, 1, 100, 1, 100);
  (2.0, AODV, 1, 100, 1, 100)
]
*)


let _ = 
 
  List.iter (fun (speed, agenttype, rate, nodes, sources,  pktsrecv) ->
    do_one_run 
    ~nodes:nodes
    ~agenttype:agenttype
    ~packet_rate:rate
    ~speed:speed
    ~sources:sources
    ~pkts_to_recv:pktsrecv
  ) runs;

  let rec print_summary l = 
    match l with 
      | (speed1, grep_pkts)::(speed2, aodv_pkts)::rem ->
	  Printf.printf "%f %d %d\n" speed1 grep_pkts aodv_pkts;
	  print_summary rem;
      | [] -> ()
      | _ -> raise (Misc.Impossible_Case  "print_Summary")
  in
  print_summary !res_summary
  

(*
used for long sessions
  (* 100 nodes *)
  (* speed routing rate nodes srcs pktsrecv *)
  (0.0, GREP, 8, 100, 10, 32000);
  (0.0, AODV, 8, 100, 10, 32000);
  (1.0, GREP, 8, 100, 10, 32000);
  (1.0, AODV, 8, 100, 10, 32000);
  (2.0, GREP, 8, 100, 10, 32000);
  (2.0, AODV, 8, 100, 10, 32000);
  (4.0, GREP, 8, 100, 10, 32000);
  (4.0, AODV, 8, 100, 10, 32000);
  (8.0, GREP, 8, 100, 10, 32000);
  (8.0, AODV, 8, 100, 10, 32000);
  (16.0, GREP, 8, 100, 10, 32000);
  (16.0, AODV, 8, 100, 10, 32000);

  (* 200 nodes *)
  (0.0, GREP, 8, 200, 20, 32000);
  (0.0, AODV, 8, 200, 20, 32000);
  (1.0, GREP, 8, 200, 20, 32000);
  (1.0, AODV, 8, 200, 20, 32000);
  (2.0, GREP, 8, 200, 20, 32000);
  (2.0, AODV, 8, 200, 20, 32000);
  (4.0, GREP, 8, 200, 20, 32000);
  (4.0, AODV, 8, 200, 20, 32000);
  (8.0, GREP, 8, 200, 20, 32000);
  (8.0, AODV, 8, 200, 20, 32000);
  (16.0, GREP, 8, 200, 20, 32000);
  (16.0, AODV, 8, 200, 20, 32000);

  (* 400 nodes *)
  (0.0, GREP, 8, 400, 20, 32000);
  (0.0, AODV, 8, 400, 20, 32000);
  (1.0, GREP, 8, 400, 20, 32000);
  (1.0, AODV, 8, 400, 20, 32000);
  (2.0, GREP, 8, 400, 20, 32000);
  (2.0, AODV, 8, 400, 20, 32000);
  (4.0, GREP, 8, 400, 20, 32000);
  (4.0, AODV, 8, 400, 20, 32000);
  (8.0, GREP, 8, 400, 20, 32000);
  (8.0, AODV, 8, 400, 20, 32000);*)
