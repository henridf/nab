(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc
open Script_utils

let reinit() = (
  Common.set_time 0.0;
  init_sched();
  init_greedy_world();
)

let nbins = 30

let targets n = match n with 
  | 500 -> 100
  | 1000 -> 1
  | 2000 -> 160
  | 4000 -> 80
  | 8000 -> 40
  | 16000 -> 20
  | 32000 -> 10
  | 64000 -> 5
  | n -> n

let nw_sizes = [
  1000;
]


let mobs = [
  new Mob.randomWalk;
]

let move_nodes ~f ~percent ~targets= (
  let iterations = ((Param.get Params.nodes) / 10) in 
  while ((proportion_met_nodes targets) < percent) do 
    repeat iterations (fun x -> 
      Nodes.iter (fun n -> if n#id >= targets then f ~node:n); 
      Common.set_time (Common.get_time() +. 1.0);
    );
    Log.log#log_notice 
      (Printf.sprintf "static prop_met_nodes %f\n"
	(proportion_met_nodes ~targets:targets));
  done;
)

let do_one_warmup_run n_nodes mob = (
  Param.set Params.nodes n_nodes;
  reinit();
  mob#initialize();
  let g = round (sqrt (i2f n_nodes)) in
  let center = (g /. 2.0, g /. 2.0) in
  let nt = targets n_nodes in
  let pers_file = Printf.sprintf 
    "warmup/mws-%dn-0.6w-%dt-%s-statictargets.mld" n_nodes nt mob#abbrevname in
  make_bler_nodes ?ntargets:(Some nt) ();
  move_nodes ~f:mob#move ~percent:0.6 ~targets:nt;
  (Nodes.node(0))#move center;
  Persistency.save_state ~node_cnt:n_nodes ~out_chan:(open_out_bin pers_file) ~ntargets:nt;
)

let do_one_routes_run n_nodes mob = (
  Param.set Params.nodes n_nodes;
  reinit();
  let nt = targets n_nodes in
  let pers_file = Printf.sprintf 
    "warmup/mws-%dn-0.6w-%dt-%s-statictargets.mld" n_nodes nt mob#abbrevname in
  let chan = open_in_bin pers_file in
  Persistency.read_state ~in_chan:chan;
  close_in chan;
  
  let mindist = 0.1 *. (sqrt (i2f (2 * n_nodes))) in
  let maxdist = ref 0.0 in
  Nodes.iter (fun src -> 
    for dst = 0 to (nt - 1) do 
      let dist = (World.w())#dist_nodes src (Nodes.node(dst)) in	  
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
  let dst = 1 in
  Nodes.iter (fun src -> 
    for dst = 0 to (nt - 1) do 
	let dist = (World.w())#dist_nodes src (Nodes.node(dst)) in	  
	if nroutes_in_bin.(whichbin dist) <= 30 then (
	  let r = do_one_route ~src:src#id ~dst:dst in
	  nroutes_in_bin.(whichbin dist) <- nroutes_in_bin.(whichbin dist) + 1;
	  let length = Route.eucl_length ~dist_f:((World.w())#dist_coords) r in
	  let cost = Route.anchor_cost r in
	  let nsearches = Route.length r in
	  datapoints.(!i) <- {Data.x=dist; Data.data=[|length; cost; (i2f nsearches)|]};
	  Printf.printf "%d\n" !i; flush stdout;
	  incr i
	)
    done
  );


  let bins = Data.binnify_f (Array.sub datapoints 0 !i) nbins in
  let results_file = Printf.sprintf 
    "results/mws-res-%dn-0.5w-%s-statictargets.txt" n_nodes mob#abbrevname in
  let chan = open_out results_file in
  Printf.fprintf chan "Distance Length Cost NSearches\n";
  Array.iter (fun bin -> 
    Printf.fprintf chan "%.2f %.2f %.2f %.2f %d\n" 
    bin.Data.center
    bin.Data.stats.(0).Data.avg
    bin.Data.stats.(1).Data.avg
    bin.Data.stats.(2).Data.avg
    bin.Data.count
  ) bins;
  
  close_out chan;

)


let do_one_radius_run n_nodes mob = (
  Param.set Params.nodes n_nodes;
  reinit();
  let nt = targets n_nodes in
  let pers_file = Printf.sprintf 
    "warmup/mws-%dn-0.6w-%dt-%s-statictargets.mld" n_nodes nt mob#abbrevname in
  let chan = open_in_bin pers_file in
  Persistency.read_state ~in_chan:chan;
  close_in chan;
  
  let mindist = 0.1 *. (sqrt (i2f (2 * n_nodes))) in
  let maxdist = ref 0.0 in
  Nodes.iter (fun src -> 
    for dst = 0 to (nt - 1) do 
      let dist = (World.w())#dist_nodes src (Nodes.node(dst)) in	  
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
  let npoints = n_nodes * (targets n_nodes) in
  let datapoints = Array.make npoints {Data.x=0.0; Data.data=[||]} in
  let i = ref 0 in
  let dst = 1 in
  Nodes.iter (fun src -> 
    for dst = 0 to (nt - 1) do 
      let dist = (World.w())#dist_nodes src (Nodes.node(dst)) in	  
      if nroutes_in_bin.(whichbin dist) <= 30 then (
 	match (src#db)#last_encounter ~nid:0 with
	  | None ->  ()
	  | Some enc ->  (
	      let our_encounter_age = Common.enc_age enc in
	      let n = 
		(World.w())#find_closest 
		src#pos 
		  (fun n -> 
		    n#id = 0 || 
		    (n#db)#encounter_age 0 < our_encounter_age)
	      in 
	      let radius = (World.w())#dist_nodes src (Nodes.node(o2v n)) in
	      
	      datapoints.(!i) <- {Data.x=dist; Data.data=[|radius|]};
	      Printf.printf "%d\n" !i; flush stdout;
	      incr i
	    )
      )
    done
  );

  let bins = Data.binnify_f (Array.sub datapoints 0 !i) nbins in
  let results_file = Printf.sprintf 
    "results/mws-res-%dn-0.5w-%s-searchradii.txt" n_nodes mob#abbrevname in
  let chan = open_out results_file in
  Printf.fprintf chan "Distance Length Cost NSearches\n";
  Array.iter (fun bin -> 
    Printf.fprintf chan "%.2f %.2f\n" 
    bin.Data.center
    bin.Data.stats.(0).Data.avg
  ) bins;
  
  close_out chan;

)

let _ = 
  Log.set_log_level ~level:Log.LOG_NOTICE;
  List.iter 
    (fun n ->  List.iter 
      (fun mob -> 
	Printf.printf "Mob %s : doing %d nodes \n" mob#abbrevname n;
	flush stdout;
	do_one_radius_run n mob) 
      mobs) 
    nw_sizes;
  cleanup()
