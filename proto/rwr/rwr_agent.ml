(*
 *
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



(** RWR Agent. 

  @author Henri Dubois-Ferriere.

*)


open Printf
open Rwr_defaults
open Rwr_pkt
open Misc

let sp = Printf.sprintf

module Rwr_stats = struct 
  type stats = {
    mutable total_xmit : int; 
    mutable data_xmit : int; 
    mutable data_orig : int; 
    mutable data_recv : int; 
    mutable adv_xmit : int; 
  }

  let create_stats() = {
    total_xmit = 0; 
    data_xmit = 0; 
    data_orig = 0; 
    data_recv = 0;
    adv_xmit = 0; 
  }

let add s1 s2 = {
  total_xmit = s1.total_xmit + s2.total_xmit; 
  data_xmit = s1.data_xmit + s2.data_xmit; 
  data_orig = s1.data_orig + s2.data_orig; 
  data_recv = s1.data_recv + s2.data_recv;
  adv_xmit = s1.adv_xmit + s2.adv_xmit; 
}


let null_stats = 
  create_stats()


end

module S = Rwr_stats

let agents_array_  =  Array.init Node.max_nstacks 
  (fun _ -> Hashtbl.create (Param.get Params.nodes))

let agent_stats ?(stack=0) nid = 
  let agent = Hashtbl.find agents_array_.(stack) nid in
  agent#stats

let reset_stats ?(stack=0) nid = 
  let agent = Hashtbl.find agents_array_.(stack) nid in
  agent#reset_stats()

let total_stats ?(stack=0) () = 
  Hashtbl.fold 
    (fun id agent tot -> S.add tot agent#stats)
    agents_array_.(stack)
    (S.create_stats())


type neighborstate = Rwr_pkt.radv 

let null_metric = {hop_dist = 0; lpl_rwr_cost=0.; lpl_cost=0.}

let hop_dist s = s.hop_dist
and lpl_rwr_cost s = s.lpl_rwr_cost
and lpl_cost s = s.lpl_cost
 
let sorted_cost 
  ?( filter : neighborstate -> bool  = (fun _ -> true))
  ( select : neighborstate -> 'a ) 
  ( neighbors : (Common.nodeid_t, neighborstate) Hashtbl.t) = 

  let cmp a b = compare (select a) (select b) in
  Array.of_list (
    List.sort cmp (Hashtbl.fold 
      (fun nid state l -> if filter state then state::l else l) neighbors []))

    

(** @param owner a [Node.node] object representing the node on which
  this agent is running *)
class rwr_agent ?(stack=0) theowner = 
object(s)

  inherit [S.stats] Rt_agent_base.base ~stack theowner 


  val mutable stats = S.create_stats()

  val mutable self_hop_dist = if theowner#id = 0 then 0 else max_int;
  val mutable self_lpl_rwr_cost = if theowner#id = 0 then 0. else max_float;
  val mutable self_lpl_cost = if theowner#id = 0 then 0. else max_float;

  val neighbors : (Common.nodeid_t, neighborstate) Hashtbl.t = 
    Hashtbl.create 10 
      
  initializer (
    s#set_objdescr 
    ~owner:(theowner :> Log.inheritable_loggable) "/RWR_Agent/";
    Hashtbl.replace agents_array_.(stack) theowner#id (s :> rwr_agent);
    (Sched.s())#sched_in ~f:s#send_hello  ~t:(Random.float rwr_HELLO_INTERVAL);
  )

    
  method neighbors = neighbors 


  method private ngbrs_by_hop_dist = 
    sorted_cost hop_dist neighbors
      
  method private ngbrs_by_lpl_rwr_cost = 
    sorted_cost lpl_rwr_cost neighbors
      
  method private ngbrs_by_lpl_cost = 
    let filter ngbrcost = ngbrcost.hop_dist < self_hop_dist in
    sorted_cost ~filter lpl_cost neighbors

  method self_state = {
    hop_dist = self_hop_dist;           
    lpl_rwr_cost = self_lpl_rwr_cost;
    lpl_cost = self_lpl_cost
}

  method private make_l3pkt ?(ttl=L3pkt.default_ttl) ?(l4pkt=`EMPTY) ~dst rwrhdr = 
    L3pkt.make_l3pkt 
      ~l3hdr:(L3pkt.make_l3hdr ~ttl ~src:myid ~dst ~ext:(`RWR_HDR rwrhdr) ())
      ~l4pkt


  method private send_hello() = (
    let now = Time.time() in

    let radv = Rwr_pkt.make_adv_hdr s#self_state in
    s#send_out (s#make_l3pkt ~dst:L3pkt.l3_bcast_addr ~ttl:1 radv);
    
    (Sched.s())#sched_in ~f:s#send_hello ~t:rwr_HELLO_INTERVAL;
  )

  (* This is called from the underlying MAC each time we receive a packet. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = (
    let rwr_hdr = L3pkt.rwr_hdr l3pkt 
    and src = L3pkt.l3src l3pkt 
    and dst = L3pkt.l3dst l3pkt 
    and ttl = L3pkt.l3ttl l3pkt in 

    begin match rwr_hdr with
      | DATA -> raise Misc.Not_Implemented;
      | ADV adv -> s#process_adv_pkt src adv
    end
  )

  method private update_lpl_rwr_metric = 
    let sorted_costs = s#ngbrs_by_lpl_rwr_cost  in
    let mincost = ref max_float in

    (* compute cost when taking into account the [n] best neighbors *)
    let cost n = 
      let tot = ref 0. in
      for i = 1 to n do
	tot := !tot +. (lpl_rwr_cost sorted_costs.(i-1));
      done;
      (1. +. !tot) /. (float n)
    in
    
    if Array.length sorted_costs > 0 then (
      
      for i = 1 to Array.length sorted_costs do 
	let c = cost i in 
	if c < !mincost then mincost := c;
      done;

      if !mincost < self_lpl_rwr_cost then (
	s#log_info (lazy (sp "Changing cost distance from %f to %f" self_lpl_rwr_cost !mincost));
	self_lpl_rwr_cost <- !mincost
      )
    )

  method private update_lpl_metric = 
    
    let sorted_costs = s#ngbrs_by_lpl_cost  in
    let mincost = ref max_float in

    (* compute cost when taking into account the [n] best neighbors *)
    let cost n = 
      let tot = ref 0. in
      for i = 1 to n do
	tot := !tot +. (lpl_cost sorted_costs.(i-1));
      done;
      (1. +. !tot) /. (float n)
    in
    
    if Array.length sorted_costs > 0 then (
      
      for i = 1 to Array.length sorted_costs do 
	let c = cost i in 
	if c < !mincost then mincost := c;
      done;

      (* unlike for other metrics, we must *always* take the best one,
	 because this of the following sequence of events:
	 - hops has not yet converged, we currently think we are at 7 hops
	 away, this allows us to use several neighbors
	 - now hops converges to 6 hops and we can use fewer neighbors; our
	 converged lpl metric should therefore be higher.
	 
	 In other words, this lpl metric depends on another metric (hops)
	 which is monotonically decreasing (assuming of course static
	 connectivity etc), where as lpl is not a monotonic function of
	 hops.
      *)
      self_lpl_cost <- !mincost 
    )

  method private process_adv_pkt src adv = 

    Hashtbl.replace neighbors src adv;
    if owner#id <> 0 then (
    if adv.hop_dist < max_int then 
      (* will need to add same check for cost when we use it *)
      if adv.hop_dist + 1 < self_hop_dist then (
	s#log_info (lazy (sp "Changing hop distance from %d to %d" self_hop_dist (adv.hop_dist + 1)));
	self_hop_dist <- adv.hop_dist + 1
      );

      if adv.lpl_cost < max_float then s#update_lpl_metric;
      if adv.lpl_rwr_cost < max_float then   s#update_lpl_rwr_metric;
    )

  method private send_out l3pkt = (
    
    let rwr_hdr = L3pkt.rwr_hdr l3pkt in
    let dst = L3pkt.l3dst l3pkt in
    assert (dst <> myid);
    assert (L3pkt.l3ttl l3pkt >= 0);

    (* a few sanity checks *)
    begin match rwr_hdr with
      | ADV adv -> assert (dst = L3pkt.l3_bcast_addr); 
	  assert(if owner#id = 0 then adv = null_metric else adv <> null_metric);
      | DATA -> ()
    end;

    stats.S.total_xmit <- stats.S.total_xmit + 1;

    match rwr_hdr with 
      | DATA -> raise Misc.Not_Implemented;
	  stats.S.data_xmit <- stats.S.data_xmit + 1;
      | ADV _ -> 
	  stats.S.adv_xmit <- stats.S.adv_xmit + 1;
	  s#mac_send_pkt l3pkt dst 
  )

  (* [app_recv_l4pkt] is the entry point from upper (L4) layers which have a 
     packet to send. We build the L3 header and originate the packet into the. *)
  method private recv_pkt_app l4pkt dst = ()


  method stats = stats
  method reset_stats() = stats <- S.create_stats()

end

let run_until_convergence ?(stack=0) () = 
  let niter = ref 0 in

  let a = Array.init (Param.get Params.nodes) (fun _ -> null_metric) in

  begin
  try 
    while true do
      let changed = 
	Hashtbl.fold 
	  (fun nid agent changed -> changed || agent#self_state <> a.(nid))
	  agents_array_.(stack) false 
      in
      if not changed then raise Break;
      
      Hashtbl.iter 
	(fun nid agent -> a.(nid) <- agent#self_state)
	agents_array_.(stack);
      
      (Sched.s())#run_for ~duration:rwr_HELLO_INTERVAL;
      
      incr niter;
    done;
  with Break -> ()
  end;
  Log.log#log_notice (lazy (sp "Converged in %d iterations\n" !niter))


let dump_neighbortable ?(stack=0) nid = 
  let agent = Hashtbl.find agents_array_.(stack) nid in
  let neighbors = agent#neighbors in
  let s = agent#self_state in 
  Printf.printf "Metrics for %d: %d hops, %.2f lpl/rwr, %.2f lpl\n" 
    nid
    (hop_dist s)
    (lpl_rwr_cost s)
    (lpl_cost  s);
  Hashtbl.iter (fun ngbr s -> 
    Printf.printf "\tneighbor %d: %d hops, %.2f lpl/rwr, %.2f lpl\n" 
    ngbr
    (hop_dist s)
    (lpl_rwr_cost s)
    (lpl_cost  s)
  ) neighbors;
  print_newline();
  flush stdout

let dump_metrics ?(stack=0) () = 
  Nodes.iteri (fun nid _ -> 
    let agent = Hashtbl.find agents_array_.(stack) nid in
    let s = agent#self_state in 
    Printf.printf "%d -> %.2f %.2f %.2f\n" nid (float s.hop_dist)
      s.lpl_rwr_cost s.lpl_cost
  );
  flush stdout
    



let make_rwr_agent ?(stack=0) n  = 
  ((new rwr_agent ~stack n) :> Rt_agent.t)
