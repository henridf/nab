(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils

let daemon = false
let outfile = ""


let outfd = ref Pervasives.stderr


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

let seed = ref 12
let nextseed() = (
  seed := !seed + 14;
  !seed
)

let res_summary = ref []

let do_one_run ~hotdest ~agenttype ~nodes ~sources ~packet_rate ~speed 
  ~pkts_to_recv = (
    (* this assumes that we alternate btw grep and aodv.*)
    if agenttype = GREP then 
      Random.init (nextseed())
    else
      Random.init !seed;

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
  Grep_hooks.set_stop_thresh (pkts_to_recv * sources);


  Nodes.iter (fun n ->
    if (n#id < sources) then (
      let dst = if hotdest then ((Param.get Params.nodes)  - 1 ) else (n#id +
      sources) in
      let pkt_reception() = n#trafficsource  dst packet_rate in
      (Gsched.sched())#sched_at ~f:pkt_reception ~t:(Sched.ASAP);
    )
  );
  
  Printf.fprintf !outfd "\n";
  Printf.fprintf !outfd "---------------------------\n";
  Printf.fprintf !outfd "\n";
  Printf.fprintf !outfd "Run %d parameters:\n" !run;
  Printf.fprintf !outfd "%d Nodes, type %s\t\t\n" nodes (agent_string agenttype);
  Printf.fprintf !outfd "Sources: %d\t\t\n" sources;
  Printf.fprintf !outfd "%d [pkt/s], %f [m/s] \n" packet_rate speed ;
  Printf.fprintf !outfd "%d [pkt bufsize]\n" Grep_agent.packet_buffer_size;
  let start_time = Common.get_time() in
(*  (Gsched.sched())#stop_in 40.0;*)
  (Gsched.sched())#run();
  

  Printf.fprintf !outfd "\n";
  Printf.fprintf !outfd "Results:\n" ;
  Printf.fprintf !outfd "Packets sent: \t%d\n" !Grep_hooks.total_pkts_sent;
  Printf.fprintf !outfd "DATA sent: \t%d\n" !Grep_hooks.data_pkts_sent;
  Printf.fprintf !outfd "DATA dropped: \t%d\n" !Grep_hooks.data_pkts_drop;
  Printf.fprintf !outfd "DATA dropped on rerr: \t%d\n" !Grep_hooks.data_pkts_drop_rerr;
  Printf.fprintf !outfd "DATA orig: \t%d\n" !Grep_hooks.data_pkts_orig;
  Printf.fprintf !outfd "DATA recv: \t%d\n" !Grep_hooks.data_pkts_recv;
  Printf.fprintf !outfd "RREQ send: \t%d\n" !Grep_hooks.rreq_pkts_sent;
  Printf.fprintf !outfd "RREP/RERR send: \t%d\n" !Grep_hooks.rrep_rerr_pkts_sent;
  Printf.fprintf !outfd "Invariant Breaks: \t%d\n" !Grep_hooks.inv_viol;

  let avgn = avg_neighbors_per_node() in
  let end_time = Common.get_time() in
  Printf.fprintf !outfd "Time elapsed is %f\n" (end_time -. start_time);
  Printf.fprintf !outfd "Avg neighbors per node is %f\n" avgn;

  res_summary := (speed, !Grep_hooks.total_pkts_sent)::!res_summary  ;
  
  flush !outfd
)



let runs = [
(* speed routing rate nodes srcs pktsrecv *)
  (* 400 nodes *)
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
  (8.0, GREP, 8, 200, 4, 200);
  (8.0, AODV, 8, 200, 4, 200);
]

(*
let runs = [
(* speed routing rate nodes srcs pktsrecv *)
  (* 400 nodes *)
  (0.0, GREP, 8, 200, 4, 100);
  (0.0, AODV, 8, 200, 4, 100);
  (1.0, GREP, 8, 200, 4, 100);
  (1.0, AODV, 8, 200, 4, 100);
  (2.0, GREP, 8, 200, 4, 100);
  (2.0, AODV, 8, 200, 4, 100);
  (4.0, GREP, 8, 200, 4, 100);
  (4.0, AODV, 8, 200, 4, 100);
  (8.0, GREP, 8, 200, 4, 100);
  (8.0, AODV, 8, 200, 4, 100);
  (16.0, GREP, 8, 200, 4, 100);
  (16.0, AODV, 8, 200, 4, 100);
]
  *)





let _ = 
 
  if daemon then (
    Script_utils.detach_daemon outfile;
    outfd := !Log.output_fd;
  );
  List.iter (fun (speed, agenttype, rate, nodes, sources,  pktsrecv) ->
    do_one_run 
    ~nodes:nodes
    ~agenttype:agenttype
    ~packet_rate:rate
    ~speed:speed
    ~sources:sources
    ~pkts_to_recv:pktsrecv
    ~hotdest:false
  ) runs;

  let aodvtot = ref 0 and greptot = ref 0 in
  let rec print_summary l = 
    match l with 
      | (speed1, aodv_pkts)::(speed2, grep_pkts)::rem ->
	  aodvtot := aodv_pkts + !aodvtot;
	  greptot := grep_pkts + !greptot;
	  Printf.fprintf !outfd "%f %d %d\n" speed1 grep_pkts aodv_pkts;
	  print_summary rem;
      | [] -> ()
      | _ -> raise (Misc.Impossible_Case  "print_Summary")
  in
  Printf.fprintf !outfd "Speed GREP AODV\n";
  print_summary !res_summary;
  Printf.fprintf !outfd "Total GREP: %d , Total AODV: %d\n" !greptot !aodvtot
  

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
