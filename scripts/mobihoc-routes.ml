(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils

let reinit() = (
  Common.set_time 0.0;
  Nodes.set_nodes [||];
  init_sched();
  init_greedy_world();

)  

let nbins = 60
let distance_range_chopoff = 0.1 (* don't look at routes within this
				    percentage of min and max distance *)

let targets n = match n with 
  | 500 -> 500
  | 1000 -> 320
  | 2000 -> 160
  | 4000 -> 80
  | 8000 -> 40
  | 16000 -> 20
  | 32000 -> 10
  | 64000 -> 5
  | n -> n

let nw_sizes = [
(*  1000;
  8000;
  16000;
  32000;*)
64000
]

let mobs = [
  new Mob.randomWalk;
  new Mob.waypoint
]

let do_one_run n_nodes mob = (
  Param.set Params.nodes n_nodes;
  reinit();
  let nt = targets n_nodes in
  let pers_file = Printf.sprintf 
    "warmup/mws-%dn-0.4w-%dt-%s.mld" n_nodes nt mob#abbrevname in
  let chan = open_in_bin pers_file in
  Persistency.read_state ~in_chan:chan;
  close_in chan;

  (* figure out what longest path is (this depends on the position of the
     targets in addition tothe size of the grid). *)
  let mindist = 0.1 *. (sqrt (i2f (2 * n_nodes))) in
  let maxdist = ref 0.0 in
  Nodes.iter (fun src -> 
    for dst = 0 to (nt - 1) do 
      let dist = (Gworld.world())#dist_nodes src (Nodes.node(dst)) in	  
      if dist > !maxdist then maxdist := dist
    done;
  );
  let nroutes_in_bin = Array.make nbins 0 in
  let binwidth = !maxdist /. (i2f nbins) in
  let whichbin num =   
    if isint (num /. binwidth) then 
      max 0 ((f2i (num /. binwidth) ) - 1) else 
	f2i (floor ((num /. binwidth))) in
  
  
  (* compute routes *)
  let nroutes = n_nodes * (targets n_nodes) in

  let datapoints = Array.make nroutes {Data.x=0.0; Data.data=[||]} in
  let i = ref 0 in
  Nodes.iter (fun src -> 
    for dst = 0 to (nt - 1) do 
      if src#id <> dst then (
	let dist = (Gworld.world())#dist_nodes src (Nodes.node(dst)) in	  

	if nroutes_in_bin.(whichbin dist) <= 400 then (
	let r = do_one_route ~src:src#id ~dst:dst in
	nroutes_in_bin.(whichbin dist) <- nroutes_in_bin.(whichbin dist) + 1;

	let length = Route.eucl_length ~dist_f:((Gworld.world())#dist_coords) r in
	let cost = Route.anchor_cost r in
	let nsearches = Route.length r in
	datapoints.(!i) <- {Data.x=dist; Data.data=[|length; cost; (i2f nsearches)|]};

	incr i
	)
      )
    done;
    Printf.printf "%d\n" !i; flush stdout;
    );


  let bins = Data.adaptive_binnify_f (Array.sub datapoints 0 !i) nbins in
  let results_file = Printf.sprintf 
    "results/mws-routes-%dn-0.5w-%s.txt" n_nodes mob#abbrevname in
  let chan = open_out results_file in
  Printf.fprintf chan "dist len stdev cost stdev nsearches stdev nsamples\n";
  Array.iter (fun bin -> 
    Printf.fprintf chan "%.2f %.2f %.2f %.2f %.2f %.2f %.2f %d\n" 
    bin.Data.center
    bin.Data.stats.(0).Data.avg
    (sqrt bin.Data.stats.(0).Data.var)
    bin.Data.stats.(1).Data.avg
    (sqrt bin.Data.stats.(1).Data.var)
    bin.Data.stats.(2).Data.avg
    (sqrt bin.Data.stats.(2).Data.var)
    bin.Data.count
  ) bins;
  
  close_out chan;

)

let () = 
  Log.set_log_level ~level:Log.LOG_NOTICE;

  List.iter 
    (fun n ->  List.iter 
      (fun mob -> 

	Printf.printf "Mob %s : doing %d nodes \n" mob#abbrevname n;
	flush stdout;
	do_one_run n mob) 
      mobs) 
    nw_sizes;
  cleanup();

