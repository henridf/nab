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


let routes_done = ref 0

let check_route_num l2pkt num_opt = 
  match num_opt with 
    | None -> true
    | Some num ->
	let l3pkt = (L2pkt.l3pkt l2pkt) in
	let l4pkt = (L3pkt.l4pkt l3pkt) in
	match l4pkt with 
	  | `APP_PKT n when (num = n) -> true
	  | `APP_PKT n -> false
	  | _ -> failwith 
	      "Ler_hooks.check_route_num: called with inappropriate packet type"


let ler_route_pktin_mhook ?num routeref l2pkt node = (

  if check_route_num l2pkt num then 
    let l3pkt = (L2pkt.l3pkt l2pkt) in
    let l3dst = L3pkt.l3dst l3pkt in
    let ler_hdr = L3pkt.ler_hdr l3pkt in
    
    match (L2pkt.l2src l2pkt) <> node#id with
      | _ -> 	(* Packet arriving at a node *)
	  (Log.log)#log_debug (lazy (Printf.sprintf "Arriving at node %d" node#id));	  
	  
	  if  node#id = l3dst then ( (* Packet arriving at dst. *)
	    incr routes_done;
	    routeref := Route.add_hop !routeref {
	      Route.hop=node#id;
	      Route.info=Some {
		Ler_route.anchor=(Ler_pkt.anchor ler_hdr);
		Ler_route.anchor_age=(Ler_pkt.enc_age ler_hdr);
		Ler_route.searchcost=0.0; (* hack see general_todo.txt *)
	      }
	    }
	  )
	    (* this should not be a failure. ie, a node can send a packet to itself, if 
	       it was closest to the previous anchor, searches for a new anchor, and is
	       closest to this anchor too *)
	    (*    | false ->  assert(false)*)
	    
)

let ler_route_pktout_mhook ?num routeref l2pkt node = (
  
  if check_route_num l2pkt num then 

  let l3pkt = (L2pkt.l3pkt l2pkt) in
  let ler_hdr = L3pkt.ler_hdr l3pkt in
  
  match (L2pkt.l2src l2pkt) <> node#id with
    | true -> 	assert(false)
    | false ->  (* Packet leaving some node *)
	
	(Log.log)#log_info (lazy (Printf.sprintf "Leaving node %d" node#id));	
	routeref := Route.add_hop !routeref {
	  Route.hop=node#id;
	  Route.info=Some {
	    Ler_route.anchor=(Ler_pkt.anchor ler_hdr);
	    Ler_route.anchor_age=(Ler_pkt.enc_age ler_hdr);
	    Ler_route.searchcost=(Ler_pkt.search_dist ler_hdr)
	  }
	}
)
