(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Coord

let set_debug_level s = 
  let level = 
    match s with 
      | "debug" -> Log.LOG_DEBUG
      | "notice" -> Log.LOG_NOTICE
      | "warning" -> Log.LOG_WARNING
      | "error" -> Log.LOG_ERROR
      | _ -> raise (Failure "Bad debug level argument")
  in
  Log.set_log_level ~level

let speclist = 
  ["-debug", Arg.String set_debug_level, "<level> Set debug level"]

let parse_args() = (
  Arg.parse speclist (fun _ -> ()) "";
)

let init_sched() = Gsched.set_sched (new Sched.schedHeap)

let init_greedy_world() = 
  Gworld.set_world (new Crworld.crworld_greedy
    ~x:(Param.get Params.x_size)
    ~y:(Param.get Params.y_size)
    ~rrange:(Param.get Params.rrange))

let init_epfl_world() = (
  if (Param.get Params.x_size) <> 800. ||
    (Param.get Params.y_size) <> 600. then
      Log.log#log_warning (lazy "For EPFL, size should be 800x600");
  Gworld.set_world (new Crworld.epflworld
    ~x:(Param.get Params.x_size)
    ~y:(Param.get Params.y_size)
    ~rrange:(Param.get Params.rrange))
)

let init_lazy_world() = 
  Gworld.set_world (new Crworld.crworld_lazy
    ~x:(Param.get Params.x_size)
    ~y:(Param.get Params.y_size)
    ~rrange:(Param.get Params.rrange)
)

let init_all() =   (
  Sched.set_time 0.0;
  init_sched(); 
  init_lazy_world()
)

let install_macs_ ~stack mac_factory = 
  Nodes.iter (fun n -> n#install_mac ~stack (mac_factory n))

let install_null_macs ?(stack=0) () = 
  install_macs_ ~stack (fun n -> new Mac_null.nullmac ~stack n)

let install_contention_macs ?(stack=0) () = 
  install_macs_ ~stack (fun n -> new Mac_contention.contentionmac ~stack n)

let install_macs ?(stack=0) () = 
  match Mac.mac() with
    | Mac.Nullmac -> install_null_macs ~stack ()
    | Mac.Contmac -> install_contention_macs ~stack ()


let make_nodes () = (
  Nodes.set_nodes [||]; (* in case this is being called a second time in the same script *)  
  Nodes.set_nodes 
    (Array.init (Param.get Params.nodes)
      (fun i -> 
	(new Simplenode.simplenode i
	)));

  install_macs  ();
  (* set up initial node position in internal structures of world object *)
  Nodes.iteri (fun nid _ -> (Gworld.world())#init_pos ~nid ~pos:(Gworld.world())#random_pos );
  assert ((Gworld.world())#neighbors_consistent);
)
    
let place_nodes_on_line () = 
  let x_incr = (Param.get Params.x_size) /.  float (Param.get Params.nodes) in
  Nodes.iteri (fun nid _ -> 
    let newpos = ((float nid) *. x_incr, 0.0) in
    (Gworld.world())#movenode ~nid ~newpos)

let make_grease_nodes () = (

  Nodes.set_gpsnodes 
    (Array.init (Param.get Params.nodes)
      (fun i -> 
	(new Gpsnode.gpsnode
	  ~pos_init:(Gworld.world())#random_pos 
	  i
	)));
  
  (* create grease agents, who will hook to their owners *)
  Ease_agent.set_agents
    (Nodes.gpsmap (fun n -> new Ease_agent.ease_agent n));

  (* set up initial node position in internal structures of world object *)
  Nodes.gpsiter (fun n -> (Gworld.world())#init_pos ~nid:n#id ~pos:n#pos);
  assert ((Gworld.world())#neighbors_consistent);
)

  

let make_grep_nodes () = (
  make_nodes();

  (* create grep agents, and 'insert' them into their owner nodes. *)
  Grep_agent.set_agents
    (Nodes.map (fun n -> new Grep_agent.grep_agent n));

  Nodes.iteri (fun nid n -> 
    n#install_rt_agent (!Grep_agent.agents_array.(nid) :> Rt_agent.t));
)

let make_diff_agents () = (
  (* create grep agents, and 'insert' them into their owner nodes. *)
  Diff_agent.set_agents
    (Nodes.map (fun n -> new Diff_agent.diff_agent n));

  Nodes.iteri (fun nid n -> 
    n#install_rt_agent (!Diff_agent.agents_array.(nid) :> Rt_agent.t));
)

let make_aodv_nodes () = (
  make_nodes();

  (* create aodv agents, and 'insert' them into their owner nodes. *)
  Aodv_agent.set_agents
    (Nodes.map (fun n -> new Aodv_agent.aodv_agent n));

  Nodes.iteri (fun nid n -> 
    n#install_rt_agent (!Diff_agent.agents_array.(nid) :> Rt_agent.t));
)




let proportion_met_nodes()  = 
  Ease_agent.proportion_met_nodes()

(*
let draw_nodes () = 
  Ler_graphics.draw_nodes (Nodes.map (fun n -> (Gworld.world())#project_2d
    n#pos))

let gui_draw_connectivity () = 
  Nodes.iter ( fun n -> 
    (List.iter
      ( fun m -> 
	Ler_graphics.ler_draw_segment [|
	  ((Gworld.world())#project_2d n#pos);
	  ((Gworld.world())#project_2d (Nodes.node(m))#pos)|] )
      ((Gworld.world())#neighbors n#id)
    )
  )
    
let draw_node ~nid = 
  Ler_graphics.draw_nodes [|((Gworld.world())#project_2d (Nodes.node(nid))#pos)|]
    
let label_node ~node = (
  Ler_graphics.label_node 
  ((Gworld.world())#project_2d node#pos)
  (Printf.sprintf "%d" node#id)
)

let label_nodes() = Nodes.iter (fun node -> label_node node)

let redraw_and_label_nodes() = (
  Ler_graphics.clear_gfx();
  draw_nodes();
  label_nodes()
)

  
*)

let avg_neighbors_per_node() = 
  let total_neighbors = 
    Nodes.fold (fun n total -> (List.length ((Gworld.world())#neighbors n#id)) + total) 0 
  in
  (i2f total_neighbors) /. (i2f (Param.get Params.nodes))

let max_neighbors_per_node() = 
  let max = ref 0 in
  let _neighbors = 
    Nodes.iter (fun n -> 
      if List.length ((Gworld.world())#neighbors n#id) > !max
      then max := List.length ((Gworld.world())#neighbors n#id)  )
  in
  max

let wait_for_any_keypress() = (
  Printf.printf "Press any key to continue...\n" ; 
  flush stdout;
  ignore (Graphics.read_key())
)

let make_app_packet ~srcid ~dstid = 
  L3pkt.make_app_pkt ~l3hdr:(L3pkt.make_l3hdr ~srcid:srcid ~dstid:dstid  ())

  


let hop_col_color ~hop = (
  [| Graphics.black; Graphics.red;
  Graphics.blue |].(hop mod 3)
)


let grep_one_route ~src ~dst = (
  let pkt_reception() = (Nodes.node(src))#originate_app_pkt dst in
  (Gsched.sched())#sched_at ~f:pkt_reception ~t:(Sched.ASAP);
  (Gsched.sched())#run();
)

(*
let gui_grep_one_route() = (

  draw_nodes();
  label_nodes();
  gui_draw_connectivity();
  let dstid = Ler_graphics.mouse_choose_node (Gworld.world())#get_node_at "choose a dest" in

  Graphics.set_color (Graphics.rgb 100 100 100);    


  let srcid = Ler_graphics.mouse_choose_node (Gworld.world())#get_node_at "choose a source" in

  let pkt_reception() = (Nodes.node(srcid))#trafficsource dstid 10 in
  (Gsched.sched())#sched_at ~f:pkt_reception ~t:(Sched.ASAP);
  (Gsched.sched())#run();

  (Nodes.node(133))#move ((Gworld.world())#random_pos);
  Ler_graphics.clear_gfx();
  draw_nodes();
  label_nodes();
  gui_draw_connectivity();

  let dstid = Ler_graphics.mouse_choose_node (Gworld.world())#get_node_at "choose a dest" in

  
  let srcid = Ler_graphics.mouse_choose_node (Gworld.world())#get_node_at "choose a source" in

  let pkt_reception() = (Nodes.node(srcid))#originate_app_pkt dstid in
  (Gsched.sched())#sched_at ~f:pkt_reception ~t:(Sched.ASAP);
  (Gsched.sched())#run();
)
*)


let move_nodes ~prop  = (
  let iterations = ( (Param.get Params.nodes) * (Param.get Params.nodes) / 10) in 
  let ctr = ref 0 in 
  let continue() = (
    incr ctr; 
    if !ctr = iterations then (
      ctr := 0;
      let p = (proportion_met_nodes()) in
      Log.log#log_notice 
	(lazy (Printf.sprintf "prop_met_nodes %f\n" p));
      p < prop 
    ) else true
  )
  in
  Mob_ctl.start_all();
  (Gsched.sched())#run_until ~continue;
  Mob_ctl.stop_all();
)
  

let print_header () = (
  Printf.printf "\n";
  Printf.printf "--------------------------------------- \n";
  Printf.printf "    mws  multihop wireless simulator    \n";
  Printf.printf "--------------------------------------- \n\n";
  Printf.printf "Simulation Parameters:\n";
  Printf.printf "\tNumber of nodes:\t\t %d\n" (Param.get Params.nodes);
  flush stdout;
)



 
let finish () = (
  Printf.printf ".. done\n";
  flush stdout;
  exit 0;
)


let detach_daemon ~outfilename = (
    let pid =  Unix.fork () in
    if pid < 0 then failwith "Error in fork";
    if pid > 0 then exit 0;
    let _ = Unix.setsid () in
    Unix.close Unix.stdin;
    Unix.close Unix.stdout;
    Unix.close Unix.stderr;
    Log.ochan := (open_out outfilename)
    
)


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
let size 
  ?(rrange=(Param.get Params.rrange))  
  ?(nodes=(Param.get  Params.nodes)) ~avg_degree () = 

  sqrt( (float nodes) *. (3.14 *. (rrange *. rrange)) /. 
    float (avg_degree))
