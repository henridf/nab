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

open Mods

type 'a info_t = {
  anchor: 'a; 
  anchor_age: Time.time_t; 
  searchcost: float}

type ('a, 'b) t = ('a, 'b info_t) Route.t


let ler_route_valid route ~src ~dst= (

  let length() =  (
    ((Route.length route >= 1) || raise (Failure "length not >= 1"))
    &&
    ((if Route.length route = 1 then (src = dst) else true) || 
    raise (Failure "length 1, src <> dst"))
  ) in
  
  let start_at_src() = 
    ((Route.nth_hop route 0).Route.hop = src) || raise (Failure "Route does not start at src") 
  in
  
  let end_at_dst() = 
    ((Route.nth_hop route (List.length route - 1)).Route.hop = dst) || raise (Failure "Route does not end at dst")
  in
  

  (* searchcost non-zero only when anchors change *)    
  let searchcost() = (
    (let rec advance front back prev_anchor = (
      match (front, back) with 
	| ([], hopb::rb) -> (
	    (* first hop: we should be sure to have an anchor. search cost may
	       be 0 if we found it locally, > 0 if we made a search. *)
	    match hopb.Route.info with 
	      | Some info ->  
		  if info.searchcost < 0.0 then false else advance [hopb] rb info.anchor
	      | None -> false
	  )
	| (hopf::rf, hopb::rb) -> (
	    match hopb.Route.info with 
	      | Some info ->  
		  (info.searchcost >= 0.0)
		  &&
		  (if (info.searchcost > 0.0) then info.anchor <> prev_anchor else true)
		  &&
		  advance (hopb::hopf::rf) rb info.anchor
	      | None -> 
		  advance (hopb::hopf::rf) rb prev_anchor

	  )
	| (_, []) -> 
	    true
    ) in 
    advance [] route (Opt.get (List.hd route).Route.info).anchor
    )
    || raise (Failure "Searchcost was non-zero without anchor change")
  ) in
  
  (* anchor_age monotonically decreasing *)
  let anchor_age_decreasing() = (
    (let rec advance r last_age = (
      
      match r with 
	| [] -> true
	| hop::nexthops -> (
	    match hop.Route.info with 
	      | Some info -> 
		  if info.anchor_age > last_age then false 
		  else advance nexthops info.anchor_age
	      | None -> advance nexthops last_age
	  )
    )  in 
    advance route max_float)
    || raise (Failure "Anchor age not monotonically decreasing")
  ) in
  
  (*
  (* anchor_age can only change when anchor changes *)
  let anchor_age_changes_with_anchor() = (
    (let rec advance front back = (
      match (front, back) with 
	| ([], hopb::rb) -> (
	    (* first hop *)
	    match hop.Route.info with 
	      | Some info -> 
		  (hopb.Route.info.anchor_age <= max_float )
		  &&
		  advance [hopb] rb 
	      | None -> false
	  )
	| (hopf::rf, hopb::rb) -> 
	    match hop.Route.info with 
	      | Some info -> (
	    (if (hopb.Route.info.anchor_age <> hopf.Route.info.anchor_age) then hopb.Route.info.anchor <> hopf.Route.info.anchor else true)
	    &&
	    advance (hopb::hopf::rf) rb
	  )
	| (rf, []) -> 
	    true
    ) in 
    advance [] route) 
    || raise (Failure "Anchor age changed but anchor did not")
  ) in
  *)
  (* loop-free (no hop is repeated twice)*)
  let loop_free() = (
    List.fold_left  
    ~f:(fun stat h ->
      (*
	print_endline (Misc.i2s (List.length (List.filter (fun k -> k.hop = h.hop) route)));
	print_endline (Misc.i2s h.hop) ;*)
      ((List.length (List.filter ~f:(fun k -> k.Route.hop = h.Route.hop) route)) = 1) && stat)
    ~init:true 
    route
  || raise (Failure "Route has a loooop")
  )
  in    
  length()
  &&
  start_at_src()
  &&
  end_at_dst()
  &&
  searchcost()
  &&
  anchor_age_decreasing()
(*  &&
  anchor_age_changes_with_anchor()
  &&
  loop_free()*)
)


let search_cost route = 
  let search_cost_at_hop hop = 
    match hop.Route.info with 
      | Some info -> info.searchcost
      | None -> 0.0
  in
  List.fold_left 
    ~f:(fun cost hop -> cost +. (search_cost_at_hop hop)) 
    ~init:0.0 
    route


let sprintnid route = (
  let printhop {Route.hop=h; Route.info=iopt} = 
    let (info : Coord.coordf_t info_t) = Opt.get iopt in
    Printf.sprintf "%s %s %s %s" 
      (Misc.padto (Misc.i2s h) 5)
      (Misc.padto (Coord.sprintf info.anchor) 20)
      (Printf.sprintf "%.3f  " info.anchor_age)
      (Printf.sprintf "%.3f  " info.searchcost)
  in

  String.concat 
  ("\n")
  ("Hop   Anchor               Age     Cost\n"::
    (List.map route ~f:printhop))
)
