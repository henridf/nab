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

let do_one_run ~agenttype ~nodes ~sources ~packet_rate ~speed
  ~pkts_to_recv = (
  Random.init 124231;
  incr run;

  Log.set_log_level ~level:Log.LOG_NOTICE;
  Param.set Params.nodes nodes;
  init_sched();
  init_world();


  begin match agenttype with
    | AODV -> make_aodv_nodes()
    | GREP -> make_grep_nodes();
  end;

  (* Attach a random waypoint mobility process to each node *)
  Mob_ctl.make_waypoint_mobs();
  Mob_ctl.start_all();
  Mob_ctl.set_speed_mps speed;

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
  (4.0, GREP, 10, 400, 1, 3200);
  (4.0, AODV, 10, 400, 1, 3200)
]
*)

let pkts_recv_per_node = 2000

let _ = 
 
  List.iter (fun (speed, agenttype, rate, nodes, sources,  pktsrecv) ->
    do_one_run 
    ~nodes:nodes
    ~agenttype:agenttype
    ~packet_rate:rate
    ~speed:speed
    ~sources:sources
    ~pkts_to_recv:pktsrecv
  ) runs
  
  
  (*
    Ler_graphics.init_gfx();
    Ler_graphics.clear_gfx();
    ignore (gui_grep_one_route());;
  *)





(*
used for long sessions
  (* 100 nodes *)
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
