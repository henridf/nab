(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)







open Printf
open Misc
open Script_utils

let reinit() = (
  Common.set_time 0.0;
  init_sched();
  init_greedy_world();
)

let nbins = 60

let targets n = match n with 
  | 500 -> 100
  | 1000 -> 320
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
  new Mobs.randomWalk;
  new Mobs.waypoint;
]






let do_one_radius_run n_nodes mob = (
  Param.set Params.nodes n_nodes;
  reinit();
  let nt = targets n_nodes in

  let pers_file = Printf.sprintf 
    "warmup/mws-%dn-0.4w-%dt-%s.mld" n_nodes nt mob#abbrevname in
  let chan = open_in_bin pers_file in
  Persistency.read_state ~in_chan:chan;
  close_in chan;
  
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
      if nroutes_in_bin.(whichbin dist) <= 500 then (
 	match (src#db)#last_encounter ~nid:dst with
	  | None ->  ()
	  | Some enc ->  (
	      let our_encounter_age = Common.enc_age enc in
	      let n = 
		(World.w())#find_closest 
		src#pos 
		  (fun n -> 
		    n#id = dst || 
		    (n#db)#encounter_age dst < our_encounter_age)
	      in 
	      let radius = (World.w())#dist_nodes src (Nodes.node(o2v n)) in
	      datapoints.(!i) <- {Data.x=dist; Data.data=[|radius|]};
	      Printf.printf "%d\n" !i; flush stdout;
	      nroutes_in_bin.(whichbin dist) <- nroutes_in_bin.(whichbin dist) + 1;
	      incr i
	    )
      )
    done
  );

  let bins = Data.binnify_f (Array.sub datapoints 0 !i) nbins in
  let results_file = Printf.sprintf 
    "results/mws-res-%dn-0.4w-%s-searchradii.txt" n_nodes mob#abbrevname in
  let chan = open_out results_file in
  Printf.fprintf chan "Distance Radius NSamples\n";
  Array.iter (fun bin -> 
    Printf.fprintf chan "%.2f %.2f %d\n" 
    bin.Data.center
    bin.Data.stats.(0).Data.avg
    bin.Data.count
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
