(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils

let daemon = true
let outfile = "/home/henridf/work/caml/R4.txt"


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
  ~pkts_to_send = (
    if agenttype = GREP then 
      Random.init (nextseed())
    else
      Random.init !seed;

  Log.set_log_level ~level:Log.LOG_ERROR;
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
  Grep_hooks.set_stop_thresh (pkts_to_send * sources);


  Nodes.iter (fun n ->
    if (n#id < sources) then (
      let dst = if hotdest then 
	((Param.get Params.nodes)  - 1 )
      else 
	(((Param.get Params.nodes)  - 1 ) - n#id)
      in
      if (dst <> n#id) then (
	(* in case we have n nodes, n sources, then the n/2'th node would have
	   itself as destination*)
	let pkt_reception() = n#trafficsource ~num_pkts:pkts_to_send ~dstid:dst ~pkts_per_sec:packet_rate in
	let start_time = Random.float 10.0 in
	(Gsched.sched())#sched_in ~f:pkt_reception ~t:start_time;
      )
    )
  );
  
  let start_time = Common.get_time() in
  (Gsched.sched())#run();
 
  let avgn = avg_neighbors_per_node() in
  let end_time = Common.get_time() in
(*  Printf.fprintf !outfd "Avg neighbors per node is %f\n" avgn;*)

  res_summary := 
  (speed, 
  !Grep_hooks.data_pkts_orig,
  !Grep_hooks.total_pkts_sent,
  !Grep_hooks.data_pkts_recv,
  !Grep_hooks.data_pkts_sent,
  !Grep_hooks.rrep_rerr_pkts_sent,
  !Grep_hooks.rreq_pkts_sent,
  !Grep_hooks.data_pkts_drop,
  !Grep_hooks.data_pkts_drop_rerr
  )::!res_summary  ;
  
  flush !outfd
)




(* R1: lcavpc23 Thursday noon (repeats 10 )
let runs = [
  (*  repeats hotspot speed rate nodes srcs pktssend *)
  (10, false,  0.0,  4,   1000,  10,  100);
  (10, false,  1.0,  4,   1000,  10,  100);
  (10, false,  2.0,  4,   1000,  10,  100);
  (10, false,  4.0,  4,   1000,  10,  100);
  (10, false,  8.0,  4,   1000,  10,  100);
  (10, false,  16.0, 4,   1000,  10,  100);
]*)


(* R2
let runs = [
(* repeats hotspot speed rate nodes srcs pktssend *)
   (10, false, 0.0,  8,   100,  40,  2400);
   (10, false,  1.0,  8,   100,  40,  2400);
   (10, false,  2.0,  8,   100,  40,  2400);
   (10, false,  4.0,  8,   100,  40,  2400);
   (10, false,  8.0,  8,   100,  40,  2400);
   (10, false,  16.0,  8,   100,  40,  2400);
]*)


(* R3 on lcavpc23 Thursday noon (repeats 6 ) 
let runs = [
  (* repeats hotspot speed rate nodes srcs pktssend *)
  (6, true,  0.0,  4,   1000,  20,  20);
  (6, true,  1.0,  4,   1000,  20,  20);
  (6, true,  2.0,  4,   1000,  20,  20);
  (6, true,  4.0,  4,   1000,  20,  20);
  (6, true,  8.0,  4,   1000,  20,  20);
  (6, true,  16.0,  4,   1000,  20,  20);
]*)

(* R4*)
let runs = [
(* repeats hotspot speed rate nodes srcs pktssend *)
   (10, true, 0.0,  8,   100,  40,  2400);
   (10, true,  1.0,  8,   100,  40,  2400);
   (10, true,  2.0,  8,   100,  40,  2400);
   (10, true,  4.0,  8,   100,  40,  2400);
   (10, true,  8.0,  8,   100,  40,  2400);
   (10, true,  16.0,  8,   100,  40,  2400);
]


let aodvtots = ref (0, 0, 0, 0, 0, 0, 0, 0)
let greptots = ref (0, 0, 0, 0, 0, 0, 0, 0)

let addtots 
  (dorig, ts, dr, ds, rreps, rreqs, dd, ddrerr)
  (sp, dorig2, ts2, dr2, ds2, rreps2, rreqs2, dd2, ddrerr2)
  = 
  (dorig + dorig2, 
  ts + ts2, 
  dr + dr2,
  ds + ds2, 
  rreps + rreps2, 
  rreqs + rreqs2, 
  dd + dd2, 
  ddrerr + ddrerr2)
  
  
let rec print_summary l = 
  match l with 
    | ((sp, dorig, ts, dr, ds, rreps, rreqs, dd, ddrerr) as aodv)
      ::
      ((sp2, dorig2, ts2, dr2, ds2, rreps2, rreqs2, dd2, ddrerr2) as grep)
      ::
      rem ->
	(*	  Printf.fprintf !outfd "%f\nTotal Sent: %d %d\nData Sent: %d %d\nRREPR Sent: %d %d\nRREQ Sent: %d %d\nDATA Recv %d %d\n" 
		  sp  ts ts2 ds ds2 rreps rreps2 rreqs rreqs2 datar datar2;*)
	aodvtots := addtots !aodvtots aodv;
	greptots := addtots !greptots grep;
	print_summary rem;
      | [] -> ()
      | _ -> raise (Misc.Impossible_Case  "print_Summary")


let _ = 
  if daemon then (
    Script_utils.detach_daemon outfile;
    outfd := !Log.output_fd;
  );

  List.iter (fun (repeats, hotspot, speed, rate, nodes, sources,  pktssend) ->
    aodvtots :=  (0, 0, 0, 0, 0, 0, 0, 0);
    greptots :=  (0, 0, 0, 0, 0, 0, 0, 0);

    incr run;
    Printf.fprintf !outfd "\n";
    Printf.fprintf !outfd "---------------------------\n";
    Printf.fprintf !outfd "\n";
    Printf.fprintf !outfd "Scenario %d parameters:\n" !run;
    Printf.fprintf !outfd "%d Nodes, %d repeats\t\t\n" nodes repeats;
    Printf.fprintf !outfd "Sources: %d\t\t\n" sources;
    Printf.fprintf !outfd "%d [pkt/s], %f [m/s] \n" rate speed ;

    Misc.repeat repeats (fun () -> 
      do_one_run 
	~nodes:nodes
	~agenttype:GREP
	~packet_rate:rate
	~speed:speed
	~sources:sources
	~pkts_to_send:pktssend
	~hotdest:hotspot;

      do_one_run 
	~nodes:nodes
	~agenttype:AODV
	~packet_rate:rate
	~speed:speed
	~sources:sources
	~pkts_to_send:pktssend
	~hotdest:false;
    );

    print_summary !res_summary;
    
    Printf.fprintf !outfd "\nResults:\n";
    let (dorig, ts, dr, ds, rreps, rreqs, dd, ddrerr) = !aodvtots in
    Printf.fprintf !outfd "DOrig TSent DRec DSent RREPS RREQS DD DDRERR\n";
    Printf.fprintf !outfd "%d %d %d %d %d %d %d %d (AODV)\n" 
      (dorig / repeats)
      (ts  / repeats)
      (dr  / repeats)
      (ds  / repeats)
      (rreps  / repeats)
      (rreqs / repeats)
      (dd  / repeats)
      (ddrerr  / repeats); 
    let (dorig, ts, dr, ds, rreps, rreqs, dd, ddrerr) = !greptots in
    Printf.fprintf !outfd "%d %d %d %d %d %d %d %d (GREP)\n" 
      (dorig / repeats)
      (ts  / repeats)
      (dr  / repeats)
      (ds  / repeats)
      (rreps  / repeats)
      (rreqs / repeats)
      (dd  / repeats)
      (ddrerr  / repeats); 
    res_summary := []
  ) runs;
  


  
  

