(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* todo:
   make mac a parameter *)

open Printf
open Misc
open Script_utils


module Pms = Params
module Pm = Param


module Config = 
struct
  let nth_top = 
    Pm.intcreate
      ~name:"nth_top"
      ~default:1
      ~cmdline:true
      ~doc:"The whichth valid topology to choose" ()

  let nsinks = 
    Pm.intcreate
      ~name:"nsinks"
      ~default:1
      ~cmdline:true
      ~doc:"Number of sinks" ()
      ~checker:(fun n -> Diff_agent.nsinks_ := Some n)

  let duration = 
    Pm.floatcreate
      ~name:"duration"
      ~default:60.
      ~cmdline:true
      ~doc:"Simulation duration" ()
      
  let difftype = 
    Pm.stringcreate ~name:"difftype" ~default:"Voronoi" ~cmdline:true
      ~doc:"Diffusion algorithm" 
      ~checker:(fun s -> Diff_agent.strset_difftype s) ()
end

let clear_rtabs() = 
  Array.iter 
  (fun agent -> Rtab.clear_all_entries ~rt:agent#get_rtab)
  !Diff_agent.agents_array

let clear_rtabs_and_build_trees ~ttl = (
  clear_rtabs();
    for i = 0 to Param.get Config.nsinks - 1 do
      let diffagent = !Diff_agent.agents_array.(i) in
      diffagent#subscribe ~ttl ()
    done;
    (Gsched.sched())#run_for ~duration:240. ;

)

(* Check if nw connected by making a global flood and verifying if all nodes
   were reached *)
let is_connected () = (

  Log.strset_log_level "warn";

  (* install loss-free mac for this test *)
  install_null_macs();
  (* Set OPP so that floods go all the way *)
  Diff_agent.diffusion_type := `OPP; 
  clear_rtabs_and_build_trees ~ttl:(Pm.get Pms.nodes);
  let count = ref 0 in
  Nodes.iteri (fun nid -> 
    
    let diffagent = !Diff_agent.agents_array.(nid) in
    let rt = diffagent#get_rtab in 
    let nexthop = Rtab.nexthop ~rt ~dst:0 in
    match nexthop with 
      | None -> ()
      | Some n -> incr count
  );
  install_contention_macs();
  clear_rtabs();
  Log.strset_log_level (Pm.get Pms.log_level);
  (* Restore diffusion type *)
  Diff_agent.strset_difftype (Pm.get Config.difftype);
  (!count = (Pm.get Pms.nodes));



)

let make_connected_world() = (
  Log.strset_log_level "warn";
  
  let rnd_state = ref (Random.get_state ()) in
  let count = ref (Pm.get Config.nth_top) in
  while !count > 0 do
    Log.lognt#log_always (lazy "Generating nodes ... ");
    init_all();
    rnd_state := Random.get_state();
    make_nodes();
    make_diff_agents();
    if is_connected() then (
      decr count;
      Log.lognt#log_always (lazy (sprintf " OK - connected graph %d more to go!" !count));
    ) else (
      Log.lognt#log_always (lazy "Not connected, trying again");
    );
    flush stdout;
  done;

(* Now we have a connected topology, we Re-generate everything with the
   previous RND seed. 
   In fact, we only need to do a init_world and then replace diff_agents
   (because the current ones are already subscribed), but this is a workaround
   because agents cannot be removed from nodes (because they stay attached to
   hooks, see general_todo.txt *)
  init_all();
  Random.set_state !rnd_state;

  make_nodes();
  make_diff_agents();

  Log.strset_log_level (Pm.get Pms.log_level);

)

let interest_tx = ref 0
let interest_tx_mhook = 
  fun l2pkt node -> 
    let l3pkt = L2pkt.l3pkt ~l2pkt in
    if L3pkt.l3grepflags ~l3pkt = L3pkt.GREP_RADV then 
    incr interest_tx

let data_tx = ref 0
let data_tx_mhook = 
  fun l2pkt node -> 
    let l3pkt = L2pkt.l3pkt ~l2pkt in
    if L3pkt.l3grepflags ~l3pkt = L3pkt.GREP_DATA then 
    incr data_tx

let install_hooks () = 
  interest_tx := 0; 
  data_tx := 0; 
  Nodes.iter (fun n -> 
    n#add_pktout_mhook interest_tx_mhook;
    n#add_pktout_mhook data_tx_mhook)

let print_stats () = (
  printf "#h interest data\n";
  printf "%d %d\n" !interest_tx !data_tx;
  flush stdout
)
  
let install_data_sources () = 
  Nodes.iter (fun n ->
    n#set_trafficsource 
    ~gen:(Tsource.make_poisson ~num_pkts:max_int ~lambda:1.)
    ~dst:0) (* dst will be ignored by diff_agent*)

let subscribe_sinks() = 
  for i = 0 to Pm.get Config.nsinks - 1 do 
    (* spread out initial interest floods *)
    let delay = Random.float 30. in
    !Diff_agent.agents_array.(i)#subscribe ~delay ();
  done    
  
let setup() = 
  
  Pm.set Pms.nodes 100;
  Pm.set Pms.rrange 8.0;
  
  Pm.set Pms.x_size (size ~avg_degree:10 ());
  Pm.set Pms.y_size (size ~avg_degree:10 ());
  
  let s = Pm.make_argspeclist () 
  in
  Myarg.parse s (fun s -> ()) "You messed up!"
    
    
    
let run_nb = ref 0

let do_one_run() = 
  Log.lognt#mark_break;
  Log.lognt#log_always (lazy (sprintf "* * * Starting run: %d" !run_nb));
  incr run_nb;
  dumpconfig stdout;
  Log.lognt#log_always (lazy "Making a connected topology");
  make_connected_world();
  
  install_hooks();
  install_null_macs();
  subscribe_sinks();
  
  (Gsched.sched())#run_for ~duration:(Pm.get Config.duration)
  
module P = Gnuplot.GnuplotArray

let plot x y = 
  let g = P.init P.X in
  P.xy g (Array.map float x) (Array.map float y)

let _ = 
  setup();
  let nsinks = [1;2;3;4;5;6;7;8;9;10] in
  let results =
    List.map 
      (fun i -> Pm.set Config.nsinks i;
	do_one_run();
	print_stats();
	!interest_tx;
      ) nsinks
  in
  plot (Array.of_list nsinks) (Array.of_list results);
  
  Printf.printf "PRinfing results, length \n";
  Misc.printlist "%d " results
  
  








(* attic *)



(*
  Nodes.iteri 
  (fun i -> 
  let closest_sinks = !Diff_agent.agents_array.(i)#closest_sinks () 
  and known_sinks = !Diff_agent.agents_array.(i)#known_sinks () in
  let msg = 
  (sprintf "Node %d known sinks: %s" i Misc.sprintlist "%d" known_sinks)
  in Log.lognt#log_always (lazy msg);
  
  let msg = (sprintf "Node %d closest sinks: %s"	i
  (sprintf "Node %d known sinks: %s" i Misc.sprintlist "%d" closest_sinks)
  in Log.lognt#log_always (lazy msg);
  )
  )
*)
