(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils

let daemon = false
let outfile = ref (Unix.getcwd())
let outfile_det = ref (Unix.getcwd())

let outfd = ref Pervasives.stderr
let outfd_det = ref Pervasives.stderr

let avg_degree = 12
let rrange = 100.

type trafficmatrix = HOTSPOT  | BIDIR | UNIDIR
type agent_type = AODV | GREP


module Config = 
struct
  let agent_of_string = function
    | "aodv" | "AODV" -> AODV
    | "grep" | "GREP" -> GREP
    | _ -> raise (Failure "Invalid format for agent type")

  let agent = Param.stringcreate
    ~name:"agent"
    ~cmdline:true
    ~default:"aodv"
    ~doc:"Routing protocol"
    ~checker:(fun s -> ignore (agent_of_string s))
    ()

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
      
  let run = Param.intcreate ~name:"run" ~default:1
    ~cmdline:true
    ~doc:"Run number" ()

  let packet_rate = 
    Param.floatcreate ~name:"rate" ~default:4.
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

    
end
  
let do_one_run ()  = (

  let agenttype = Config.agent_of_string (Param.get Config.agent)
  and sources = (Param.get Config.sources)
  and speed = (Param.get Config.speed)
  and pkts_to_send = (Param.get Config.pktssend)
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
	  n#set_trafficsource 
	    ~gen:(Tsource.make_cbr
	      ~pkts_per_sec:(Param.get Config.packet_rate)
	      ()
	    )
	    ~dst 
	in

	(Gsched.sched())#sched_at ~f:pkt_reception ~t:Sched.ASAP;
      )
    )
  );
  
  (Gsched.sched())#run();
  
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
  
  Printf.printf "#h proto run n rate srcs tmat speed DOrig TSent DRec DSent RREPS RREQS DD DDRERR\n";
  flush stdout;
  do_one_run ();




  


  
  

