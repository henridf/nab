(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils
open Grep_common

let daemon = false
let outfile = ref (Unix.getcwd())
let outfile_det = ref (Unix.getcwd())

let outfd = ref Pervasives.stderr
let outfd_det = ref Pervasives.stderr

let avg_degree = 12
let rrange = 100.



  
let do_one_run ()  = (

  let agenttype = Config.agent_of_string (Param.get Config.agent)
  and sources = (Param.get Config.sources) 
  and speed = (Param.get Config.speed)
  and pkts_to_send = (Param.get Config.pktssend)
  and rrange = (Param.get Params.rrange)
  in

  Randoms.change_seed ~newseed:(Param.get Config.run) () ;

  Param.set Params.x_size 
    (size ~rrange ~avg_degree	~nodes:(Param.get Params.nodes) ());
  Param.set Params.y_size 
    (size ~rrange ~avg_degree	~nodes:(Param.get Params.nodes) ());
  
  init_sched();
  init_world();
  
  begin match agenttype with
    | AODV -> make_aodv_nodes()
    | GREP -> make_grep_nodes();
  end;

  (* Attach a random waypoint mobility process to each node *)
  Mob_ctl.make_waypoint_mobs ~gran:(rrange /. 10.) ();
  Mob_ctl.set_speed_mps speed;
  Mob_ctl.start_all();

  Grep_hooks.set_sources sources;
  Grep_hooks.set_stop_thresh (pkts_to_send * sources);

  Grep_common.install_tsources();

  
  (Gsched.sched())#run_for 200.;
  
  let avgn = avg_neighbors_per_node() in
  let end_time = Common.get_time() in
  (*  Printf.fprintf !outfd "# Avg neighbors per node is %f\n" avgn;*)

  let (agent, sp, dorig, ts, dr, ds, rreps, rreqs, dd, ddrerr) = 
    (Param.get Config.agent,
    speed, 
    !Grep_hooks.data_pkts_orig,
    !Grep_hooks.total_pkts_sent,
    !Grep_hooks.data_pkts_recv,
    !Grep_hooks.data_pkts_sent,
    !Grep_hooks.rrep_rerr_pkts_sent,
    !Grep_hooks.rreq_pkts_sent,
    !Grep_hooks.data_pkts_drop,
    !Grep_hooks.data_pkts_drop_rerr
    )
  in
  Printf.printf  "%s %d %d %.1f %d %s %.1f %d %d %d %d %d %d %d %d \n" 
    agent 
    (Param.get Config.run)
    (Param.get Params.nodes)
    (Param.get Config.packet_rate)
    sources
    (Param.get Config.tmat)
    speed
    dorig
    ts
    dr
    ds
    rreps
    rreqs
    dd
    ddrerr;
  flush stdout
)

let setup() = 
  
  Param.set Params.nodes 100;
  Param.set Params.rrange rrange;
  
  let s = Param.make_argspeclist () 
  in
  Myarg.parse s (fun s -> ()) "You messed up!"
    

let _ = 

  if daemon then (
    Script_utils.detach_daemon !outfile;
    outfd := !Log.ochan;
  );
  setup();
  
(*  Param.printconfig stdout;*)
  
  if (Param.get Config.run) = 1 then
    Printf.printf "#h proto run n rate srcs tmat speed DOrig TSent DRec DSent RREPS RREQS DD DDRERR\n";
  flush stdout;
  do_one_run ();




  


  
  

