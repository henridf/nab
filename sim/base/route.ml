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



open Mods



(* Simple list-based representation of a LER route. *)


open Common
open Printf

type ('a, 'b) hop_t = {hop:'a; mutable info:'b option}

type 'a ease_info_t = {
  anchor: 'a; 
  anchor_age: Time.time_t; 
  searchcost: float}

type grep_info_t = Flood.t



type ('a, 'b) t = ('a, 'b) hop_t list

type ('a, 'b) ease_route_t = ('a, 'b ease_info_t) t
type 'a grep_route_t = ('a, grep_info_t) t
type 'a hops_only_route_t = ('a, unit) t



let create () = []


let add_hop path hop = path @ [hop]
  (* according to lib/ocaml/list.mli, non-tail recursion can be ignored when <
     10000 elements, which a route is almost certain be *)

let append_hops ~front ~back = front @ back

let nth_hop path n = List.nth path n
let last_hop path = Misc.listlast path

let length path = List.length path


let i2c route = (
  List.map 
  (fun h -> 
    {h with hop=(World.w())#nodepos h.hop})
    route
)


let ease_route_valid route ~src ~dst= (

  let length() =  (
    ((length route >= 1) || raise (Failure "length not >= 1"))
    &&
    ((if length route = 1 then (src = dst) else true) || 
    raise (Failure "length 1, src <> dst"))
  ) in
  
  let start_at_src() = 
    ((nth_hop route 0).hop = src) || raise (Failure "Route does not start at src") 
  in
  
  let end_at_dst() = 
    ((nth_hop route (List.length route - 1)).hop = dst) || raise (Failure "Route does not end at dst")
  in
  

  (* searchcost non-zero only when anchors change *)    
  let searchcost() = (
    (let rec advance front back prev_anchor = (
      match (front, back) with 
	| ([], hopb::rb) -> (
	    (* first hop: we should be sure to have an anchor. search cost may
	       be 0 if we found it locally, > 0 if we made a search. *)
	    match hopb.info with 
	      | Some info ->  
		  if info.searchcost < 0.0 then false else advance [hopb] rb info.anchor
	      | None -> false
	  )
	| (hopf::rf, hopb::rb) -> (
	    match hopb.info with 
	      | Some info ->  
		  (info.searchcost >= 0.0)
		  &&
		  (if (info.searchcost > 0.0) then info.anchor <> prev_anchor else true)
		  &&
		  advance (hopb::hopf::rf) rb info.anchor
	      | None -> 
		  advance (hopb::hopf::rf) rb prev_anchor

	  )
	| (rf, []) -> 
	    true
    ) in 
    advance [] route (Opt.get (List.hd route).info).anchor
    )
    || raise (Failure "Searchcost was non-zero without anchor change")
  ) in
  
  (* anchor_age monotonically decreasing *)
  let anchor_age_decreasing() = (
    (let rec advance r last_age = (
      
      match r with 
	| [] -> true
	| hop::nexthops -> (
	    match hop.info with 
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
	    match hop.info with 
	      | Some info -> 
		  (hopb.info.anchor_age <= max_float )
		  &&
		  advance [hopb] rb 
	      | None -> false
	  )
	| (hopf::rf, hopb::rb) -> 
	    match hop.info with 
	      | Some info -> (
	    (if (hopb.info.anchor_age <> hopf.info.anchor_age) then hopb.info.anchor <> hopf.info.anchor else true)
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
      ((List.length (List.filter (fun k -> k.hop = h.hop) route)) = 1) && stat)
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
  &&
(*  anchor_age_changes_with_anchor()
  &&*)
  loop_free()
)

	  
let eucl_length ~dist_f route = (
  let rec recurse_ r len = 
    match r with 
      | [dst] -> len
      | a::b::c -> recurse_ (b::c) (len +. (dist_f a.hop b.hop))
      | [] -> raise (Misc.Impossible_Case "Route.eucl_length")
  in
recurse_ route 0.0
)


let search_cost route = 
  let search_cost_at_hop hop = 
    match hop.info with 
      | Some info -> info.searchcost
      | None -> 0.0
  in
  List.fold_left 
    ~f:(fun cost hop -> cost +. (search_cost_at_hop hop)) 
    ~init:0.0 
    route

let sprint route = (
  String.concat 
  ("\n")
  (List.map
    (fun x -> Printf.sprintf "hop:%s" 
      (Coord.sprintf x.hop) )
    route)
)  
  
