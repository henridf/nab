(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils
open Experiment

let outfile = ref ((Unix.getcwd())^"/tmp.txt")
let outfile_det = ref ((Unix.getcwd())^"/tmp.txt")

let avg_degree = 12
let rrange = 100.

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

      

    


type trafficmatrix = HOTSPOT  | BIDIR | UNIDIR
type agent_type = AODV | GREP

let string_of_agent t =  match t with | AODV -> "AODV" | GREP -> "GREP"

module Config = 
struct
  let string_of_tmat tmat = 
    match tmat with 
      | HOTSPOT -> "hotspot "
      | BIDIR  -> "bidirectional"
      | UNIDIR -> "unidirectional"

  let tmat_of_string tmat = 
    match (String.lowercase tmat) with 
      | "hotspot" | "hot" -> HOTSPOT
      | "bidirectional" | "bi" | "bidir" -> BIDIR  
      | "unidirectional" | "uni" | "unidir" -> UNIDIR  
      | _ -> raise (Failure "Invalid format for traffic type")

  let tmat = 
    Param.stringcreate  ~name:"tmat" ~default:"Hotspot" 
      ~cmdline:true
      ~doc:"Traffic Type" ~checker:(fun s -> ignore (tmat_of_string s))
      ()

  let sources = 
    Param.intcreate  ~name:"sources" ~default:1
      ~cmdline:true
      ~doc:"Number of sources"  ()
      
  let packet_rate = 
    Param.intcreate ~name:"rate" ~default:4
      ~cmdline:true
      ~doc:"Orig rate [pkt/s]"  ()

  let speed = 
    Param.floatcreate ~name:"speed" 
      ~cmdline:true
      ~doc:"Node Speed [m/s]"  ()

  let pktssend = 
    Param.intcreate ~name:"pktssend" 
      ~cmdline:true
      ~doc:"Packets originated"  ()

  let agent_of_string = function
    | "aodv" | "AODV" -> AODV
    | "grep" | "GREP" -> GREP
    | _ -> raise (Failure "Invalid format for agent type")

  let agent = Param.stringcreate
    ~name:"agent"
    ~default:"GREP"
    ~cmdline:true
    ~doc:"Traffic Type"
    ~checker:(fun s -> ignore (agent_of_string s))
    ()
end

let do_one_run() = (

    let agenttype = Config.agent_of_string (Param.get Config.agent)
    and sources = (Param.get Config.sources)
    and speed = (Param.get Config.speed)
    and pkts_to_send = (Param.get Config.pktssend)
    in

    Rnd_manager.change_seed();

    Param.set Params.x_size 
      (size ~rrange ~avg_degree	~nodes:(Param.get Params.nodes));
    Param.set Params.y_size 
      (size ~rrange ~avg_degree	~nodes:(Param.get Params.nodes));
    
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
	let dst = 
	  match Config.tmat_of_string (Param.get Config.tmat) with
	    | HOTSPOT -> ((Param.get Params.nodes)  - 1 )
	    | UNIDIR -> (((Param.get Params.nodes)  - 1 ) - n#id)
	    | BIDIR -> (sources - n#id)
	in
	if (dst <> n#id) then (
	  (* in case we have n nodes, n sources, then the n/2'th node would have
	     itself as destination*)
	  let pkt_reception() = 
	    n#trafficsource 
	      ~num_pkts:pkts_to_send 
	      ~dst 
	      ~pkts_per_sec:(Param.get Config.packet_rate) in
	  let start_time = Random.float 10.0 in
	  (Gsched.sched())#sched_in ~f:pkt_reception ~t:start_time;
	)
      )
    );
    
    let start_time = Common.get_time() in
    (Gsched.sched())#run();
    
    let avgn = avg_neighbors_per_node() in
    let end_time = Common.get_time() in
    (*  Printf.fprintf !outfd "# Avg neighbors per node is %f\n" avgn;*)


    (speed, 
    !Grep_hooks.data_pkts_orig,
    !Grep_hooks.total_pkts_sent,
    !Grep_hooks.data_pkts_recv,
    !Grep_hooks.data_pkts_sent,
    !Grep_hooks.rrep_rerr_pkts_sent,
    !Grep_hooks.rreq_pkts_sent,
    !Grep_hooks.data_pkts_drop,
    !Grep_hooks.data_pkts_drop_rerr
    );
  )


let () = 

(* radio range stays constant; area size changes 
   (to have different connectivity) *)
  Param.set Params.rrange rrange;
  Log.set_log_level ~level:Log.LOG_WARNING;

  let repeats = 1 in  
  let runner() = (
    Script_utils.dumpconfig stdout;
    do_one_run ();
  ) in
  

  let printer 
    (sp, dorig_a, ts_a, dr_a, ds_a, rreps_a, rreqs_a, dd_a, ddrerr_a) 
    = 
    Printf.sprintf "# %d %d %d %d %d %d %d %d" 
      (dorig_a / repeats)
      (ts_a / repeats)
      (dr_a / repeats)
      (ds_a / repeats)
      (rreps_a / repeats)
      (rreqs_a / repeats)
      (dd_a  / repeats)
      (ddrerr_a  / repeats); 
    
  in 
  let rcfg = 
    "-nodes 100 -agent GREP -sources 10  -tmat Uni -rate 4 -agent GREP -pktssend 10" in
  let scfg = {runconfig=rcfg; repeats=1} in
  let ecfg = 
    {series=scfg; variable_param="-speed"; param_values=[|"4"; "8"; "12"|]} in

  let results = doexperiment ecfg runner  in
  let plotline1 = get_plotline "totalsent1" results 
    (function  (sp, dorig2, ts2, dr2, ds2,  rreps2, rreqs2, dd2, ddrerr2) ->
	  float ts2 ) in

  Experiment.print_exp results printer;

  let results = doexperiment ecfg runner  in
  let plotline2 = get_plotline "totalsent2" results 
    (function  (sp, dorig2, ts2, dr2, ds2,  rreps2, rreqs2, dd2, ddrerr2) ->
	  float ts2) in
  Experiment.print_exp results printer;

  Rnd_manager.init_seed();

  let rcfg = 
    "-nodes 100 -agent GREP -sources 10  -tmat Uni -rate 4 -agent GREP -pktssend 10" in

  let scfg = {runconfig=rcfg; repeats=2} in
  let ecfg = 
    {series=scfg; variable_param="-speed"; param_values=[|"4"; "8"; "12"|]} in
  let results = doexperiment ecfg runner  in
  let plotline_avg = get_plotline "totalsent_avg" results 
    (function (sp, dorig2, ts2, dr2, ds2,  rreps2, rreqs2, dd2, ddrerr2) ->
	  float ts2) in
  
  Printf.printf  "# DOrig TSent DRec DSent RREPS RREQS DD DDRERR\n";
  Experiment.print_exp results printer;

  print_plotline plotline1;
  print_plotline plotline2;
  print_plotline plotline_avg;

  let sum = Array.mapi (fun i (x1, y1)  -> 
    let (x2, y2) = plotline2.linedata.(i) in
    Printf.printf "asserting 1\n";
    assert (x1 = x2); (x1, y1 +. y2))
    plotline1.linedata  in

  Array.iteri  (fun i (x1, y1) -> 
    let (x2, y2) = plotline_avg.linedata.(i) in
    Printf.printf "asserting %f = %f\n" y1 y2;
    assert (x1 = x2); assert (y1 /. 2. = y2)
  ) sum;

  exit 0;  
    
(*
    Printf.fprintf !outfd "\n#Results:\n";
    let (dorig_a, ts_a, dr_a, ds_a, rreps_a, rreqs_a, dd_a, ddrerr_a) = !aodvtots in
    Printf.fprintf !outfd "# DOrig TSent DRec DSent RREPS RREQS DD DDRERR\n";
    Printf.fprintf !outfd "# %d %d %d %d %d %d %d %d (AODV)\n" 
      (dorig_a / repeats)
      (ts_a / repeats)
      (dr_a / repeats)
      (ds_a / repeats)
      (rreps_a / repeats)
      (rreqs_a / repeats)
      (dd_a  / repeats)
      (ddrerr_a  / repeats); 
*)

  


  
  

