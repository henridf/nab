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







(* todo: make mac a parameter *)

open Printf
open Misc
open Script_utils
open Voronoi_common

module Pms = Params
module Pm = Param

let (///.) = (Coord.(///.))

let sinks = ref ([||]:int array)
let choose_sinks() = 
  sinks := Array.init (Param.get Config.nsinks ) id
(*  (World.w())#movenode ~nid:0 ~newpos:((0., (Pm.get Pms.y_size) /. 2.));
  (World.w())#movenode ~nid:1 ~newpos:((Pm.get Pms.x_size, (Pm.get Pms.y_size) /. 2.))*)
(*
  These two variants were for putting two sinks att opposite ends of nw , or
  or right next to each other (in order to see difference btw ttl and voronoi).

  let s1 =  (World.w())#find_closest ~pos:(0.0, 0.0) ~f:(const true)
  and s2 =  (World.w())#find_closest ~pos:(Pm.get Pms.x_size, Pm.get Pms.y_size) ~f:(const true)
  in sinks := [|o2v s1; o2v s2|];

  let s1 =  (World.w())#find_closest 
    ~pos:((Pm.get Pms.x_size, Pm.get Pms.y_size) ///. 2.) 
    ~f:(const true) in
  let s2 =  (World.w())#find_closest 
    ~pos:((Pm.get Pms.x_size, Pm.get Pms.y_size) ///. 2.) 
    ~f:(fun n -> n <> (o2v s1))
  in sinks := [|o2v s1; o2v s2|];

  assert ((Param.get Config.nsinks ) = 2)
*)


let s i = !sinks.(i)
let is_sink i = array_mem !sinks i


let foreach_sink f = Array.iter f !sinks

let renew_agents() = 
  Nodes.iter (fun n -> n#remove_rt_agent());
make_diff_agents()
  
let clear_rtabs() = 
  Array.iter 
  (fun agent -> Rtab.clear_all_entries ~rt:agent#get_rtab)
  !Diff_agent.agents_array

let clear_rtabs_and_build_one_tree ~ttl = (
  clear_rtabs();
  let diffagent = !Diff_agent.agents_array.(s 0) in
  diffagent#subscribe ~one_shot:true ~ttl ();
  (Sched.s())#run() ;
)

(* Check if nw connected by making a global flood and verifying if all nodes
   were reached. *)
let is_connected () = (

(*  Log.strset_log_level "warn";*)

  (* Set OPP so that floods go all the way *)
  Diff_agent.diffusion_type := `OPP; 
  clear_rtabs_and_build_one_tree ~ttl:(Pm.get Pms.nodes);
  let count = ref 0 in
  Nodes.iteri (fun nid _ -> 
    
    let diffagent = !Diff_agent.agents_array.(nid) in
    let rt = diffagent#get_rtab in 
    let nexthop = Rtab.nexthop ~rt ~dst:(s 0) in
    match nexthop with 
      | None -> ()
      | Some n -> incr count
  );

  clear_rtabs();
  Log.strset_log_level (Pm.get Pms.log_level);
  (* Restore diffusion type *)
  Diff_agent.strset_difftype (Pm.get Config.difftype);
  (!count = (Pm.get Pms.nodes));
)

let make_connected_world() = (
  (* set the seed back to same value for generating topology *)

  Log.strset_log_level "warn";
  
  let exitloop = ref false in

  while not !exitloop do
    Log.lognt#log_info (lazy "Generating nodes ... ");
    init_all();
    make_nodes();
    make_diff_agents();

    if is_connected() then (
      exitloop := true;
      Log.lognt#log_notice (lazy (sprintf " OK - connected graph !" ));
    ) else (
      Log.lognt#log_notice (lazy "Not connected, trying again");
    );
  done;

(* we now have a good topology *)

    Log.strset_log_level (Pm.get Pms.log_level);

)

let interest_tx = ref 0
let interest_tx_mhook = 
  fun l2pkt node -> 
    let l3pkt = L2pkt.l3pkt ~l2pkt in
    if L3pkt.l3grepflags ~l3pkt = L3pkt.GREP_RADV then 
    incr interest_tx


let install_hooks () = 
  interest_tx := 0; 
  Nodes.iter (fun n -> n#clear_pkt_mhooks);
  Nodes.iter (fun n -> n#add_pktout_mhook interest_tx_mhook)


let print_stats () = (
  let params = Param.configlist() in
  printf "#h interest ";
  List.iter (fun (name, value) -> print_string (name^" ")) params;
  printf "\n";
  printf "%d " !interest_tx;
  List.iter (fun (name, value) -> print_string (value^" ")) params;
  printf "\n";
  flush stdout
)

let subscribe_sinks() = 
  for i = 0 to Pm.get Config.nsinks - 1 do 
    (* spread out initial interest floods *)
    (Sched.s())#sched_in  ~f:(!Diff_agent.agents_array.(s i)#subscribe ~one_shot:true
      ) ~t:(((float i) *. 30.) +. 1.);
  done

let unsubscribe_sinks() = 
  for i = 0 to Pm.get Config.nsinks - 1 do 
    (* spread out initial interest floods *)
    !Diff_agent.agents_array.(s i)#unsubscribe ;
  done
  
let send_one_round_interests() = 
  subscribe_sinks();
  (Sched.s())#run()

let setup() = 

  Pm.set Pms.rrange 8.0;
  
  let s = Pm.make_argspeclist ()
  in
  Arg.parse s (fun s -> ()) "You messed up!";

  Pm.set Pms.x_size (size ~avg_degree:10 ());
  Pm.set Pms.y_size (size ~avg_degree:10 ());
  choose_sinks()
  
    
    
let run_nb = ref 0

let voronoi_trial() = 
  Param.set Config.difftype "vor";
  renew_agents();
  send_one_round_interests();
  install_hooks();
  send_one_round_interests()

let ttl_trial () = 
  Param.set Config.difftype "opp";
  renew_agents();
  send_one_round_interests();
  let arr = Array.make (Pm.get Pms.nodes) 0 in
  
  for i = 0 to (Pm.get Pms.nodes) - 1 do
    if not (is_sink i) then begin
      let agent = !Diff_agent.agents_array.(i) in
      let closest_sink = Misc.rnd_from_list (agent#closest_sinks()) in
      let distance_to_sink = Misc.o2v (Rtab.hopcount ~dst:closest_sink ~rt:agent#get_rtab) in
      arr.(closest_sink) <- max arr.(closest_sink) distance_to_sink;
    end
  done;

  install_hooks();  

  for i = 0 to Pm.get Config.nsinks - 1 do 
    (Sched.s())#sched_in  
    ~f:(!Diff_agent.agents_array.(s i)#subscribe  ~one_shot:true  ~ttl:arr.(s i) ) 
      ~t:(((float i) *. 30.) +. 1.);
  done;
  (Sched.s())#run()



let do_one_run() = 
  Log.lognt#mark_break;
  Log.lognt#log_info (lazy (sprintf "* * * Starting run: %d" !run_nb));
  incr run_nb;
  Param.printconfig !Log.ochan;
  Log.lognt#log_info (lazy "Making a connected topology");
  make_connected_world();
  voronoi_trial();
  print_stats();
  ttl_trial();
  print_stats()






  
let () = 
  setup();
  for i = 0 to (Pm.get Config.nruns) - 1 do
  do_one_run()
done
