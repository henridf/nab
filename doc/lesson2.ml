(*
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL).
 *
 *)

(* $Id$ *)

let nodes = 900
let rrange = 12.0
let avg_degree = 10

let nstacks = 3

let originator = ref 0 (* the node from which we will originate floods. *)

let flood_trees = ref [||] (* Array of refs to flood trees - gets populated
			      below. *)

let stack_descriptions = 
  [|
    "Null MAC, 100Kbps";
    "Contention MAC, 100Kbps, 0.1s jitter";
    "Contention MAC, 200Kbps, 3s jitter"
  |]
  
  

let setup() = (
  Param.set Params.nodes nodes;
  
  Param.set Params.radiorange rrange;
  
  let size = Script_utils.size ~rrange ~nodes ~avg_degree () in
  
  Param.set Params.x_size size;
  Param.set Params.y_size size;
  
  Script_utils.init_world();
  
  Script_utils.make_naked_nodes ();

  Script_utils.install_null_macs ~bps:1e5 ~stack:0 ();
  Script_utils.install_contention_macs ~bps:1e5 ~stack:1 ();
  Script_utils.install_contention_macs ~bps:2e5 ~stack:2 ();

  Hashtbl.iter (fun nid mac -> mac#set_jitter 3.0) (Mac_contention.macs ~stack:2 ());
  
  for stack = 0 to nstacks - 1 do
    Script_utils.make_flood_agents ~stack ();
  done
)


(* 
   Adds basic_stats for all macs on given stack, and returns a basic_stats
   data structure containing the totals. 
   For ocaml newbies, a more c-like version of this is given below
   add_basic_stats_imperative.
*)
let add_basic_stats stack = 
  Hashtbl.fold 
    (fun nodeid mac stats -> Mac_base.add_bstats mac#basic_stats stats)
    (Mac_base.macs ~stack ())
    (Mac_base.zero_bstats())

(* Same as add_basic_stats, but written in a c-like style. 
   Provided for illustrative purposes, but you should really use iterators as
   in add_basic_stats above!!
*)
let add_basic_stats_imperative stack = 
  let totals = ref (Mac_base.zero_bstats()) in
  let macs = (Mac_base.macs ~stack ()) in
  for nodeid = 0 to Param.get Params.nodes - 1 do
    let mac = Hashtbl.find macs nodeid 
    in totals := Mac_base.add_bstats !totals mac#basic_stats
  done;
  !totals
    
(* 
   Adds contention-mac specific stats for all macs on given stack, and returns
   a Mac_contention.other_stats  data structure containing the totals. 
*)
let add_cont_stats stack = 
  Hashtbl.fold 
    (fun nodeid mac stats -> Contention_frontend.add_ostats mac#other_stats stats)
    (Mac_contention.macs ~stack ())
    (Contention_frontend.zero_ostats())


(* 
   This function will be called each time a packet is received by a node.
   Parameters passed to the function are the packet itself and the node
   receiving it.
   When this happens we extend the tree representing the flood, adding a child
   node (receiving node) to the node which send the packet.
*)
let tree_hook flood_tree l2pkt node = (
  let l2src = (L2pkt.l2src l2pkt) in
  flood_tree := 
  try (Flood.addnode  ~parent:l2src ~node:node#id !flood_tree)
  with (Failure "addnode") -> !flood_tree
    (* Adding a node can raise an exception if the node is already in the
       tree. When that happens, it means that our node is receiving the flood
       packet for the second (or more) time, so we don't care. *)
)

(* Install the hooks on each stack to reconstruct the flood tree. *)
let setup_hooks() = (
  flood_trees := Array.init nstacks (fun _ -> ref (Flood.create !originator));

  for stack = 0 to nstacks - 1  do

    Nodes.iter (fun n -> n#clear_pkt_mhooks ~stack ());
    Nodes.iter (fun n -> n#add_pktin_mhook ~stack (tree_hook !flood_trees.(stack)));

  done;

)

let dist_to_originator n = 
  let orig_pos = ((World.w())#nodepos !originator) 
  and n_pos = ((World.w())#nodepos n) 
  in 
  Coord.dist_sq  orig_pos n_pos;;

let is_backward child parent = 
  dist_to_originator child < dist_to_originator parent

let count_backward_links tree = 
  let ctr = ref 0 in 
  NaryTree.iter2 
    ~f:(fun ~parent  ~child -> if is_backward child parent then incr ctr)
    tree;
  !ctr


let do_flood() = (
  (* Select the node closest to the center to be the originator. *)
  let center_x = (Param.get Params.x_size) /. 2.0
  and center_y = (Param.get Params.y_size) /. 2.0  in
  originator := Opt.get ((World.w())#find_closest ~pos:(center_x, center_y) ());

  (* Insert hooks to reconstruct the flood trees. *)
  setup_hooks();

  (* Inject a packet into the originating node. *)
  (Nodes.node !originator)#originate_app_pkt ~l4pkt:(`APP_PKT 0) ~dst:L3pkt.l3_bcast_addr;

  (* Run forrest run! *)
  (Sched.s())#run()
)


let print_info() = (
  let avg_ngbrs = Script_utils.avg_neighbors_per_node() in

  let pr = Printf.sprintf in
  Printf.printf "Average node degree: %f\n" avg_ngbrs;

  for stack = 0 to nstacks - 1 do 
    Printf.printf "Stack %d (%s) \n\n" stack stack_descriptions.(stack);

    let totals = add_basic_stats stack in
    Printf.printf "\t%s\n" (Mac_base.string_of_bstats_pkts totals);


    if stack <> 0 then ( (* null mac doesn't have additional stats *)
      let totals = add_cont_stats stack in
      Printf.printf "\t%s\n" (Contention_frontend.string_of_ostats totals);
    );

    let backlinks = (count_backward_links !(!flood_trees.(stack))) in
    Printf.printf "\t%d backward links\n\n" backlinks;
      
    print_newline();
  done



)

(* This is simply there so that we can invoke this program with -script and
   have it exit immediately in automated tests. *)
let script = Param.boolcreate 
  ~name:"script" 
  ~doc:"exit immediately (for scripted tests)"
  ~cmdline:true
  ~default:false
  ~notpersist:true ()


let main = 
  Log.set_log_level Log.LOG_NOTICE;
  setup();
  do_flood();

  print_info()

