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

open Misc

let routes_done = ref 0
 
let find_last_flood route = 
  let n = ref None in
  for i = (List.length route) - 1 downto 0 do
    if (List.nth route i).Route.info <> None then
      if !n = None then
	n := Some i
  done;
  !n


type u = [ Aodv_pkt.aodv_flags_t | Grep_pkt.grep_flags_t | `NONE]
    
let aodv_grep_flags l3pkt = 

  match L3pkt.l3hdr_ext l3pkt with 
    | `AODV_HDR h -> ((Aodv_pkt.flags h) :> u)
    | `GREP_HDR h -> ((Grep_pkt.flags h) :> u)
    | _ -> `NONE 


let od_route_pktin_mhook routeref l2pkt node = (
  
  let l3pkt = (L2pkt.l3pkt l2pkt) in
  let l3dst = L3pkt.l3dst l3pkt

  and l2src = (L2pkt.l2src l2pkt) in

  if (l2src = node#id) then failwith "Od_hooks.od_route_pktin_mhook";

  match aodv_grep_flags l3pkt with
    | `DATA ->
	(Log.log)#log_debug (lazy (Printf.sprintf "Arriving at node %d" node#id));	  
	if  node#id = l3dst then ( (* Packet arriving at dst. *)
	  incr routes_done;
	  routeref := Route.add_hop !routeref {
	    Route.hop=node#id;
	    Route.info=None
	  }
	)
    | `RREQ  ->
	assert (Route.length !routeref > 0);
	let hopno = find_last_flood !routeref in
	assert (hopno <> None);
	let tree = o2v (Route.nth_hop !routeref (o2v hopno)).Route.info in
	assert (NaryTree.belongs l2src tree);

	let newtree = 
	  (* A flood is not a tree, so this node may receive the rreq more
	     than once. we only care for the first time.*)
	  try (Flood.addnode  ~parent:l2src ~node:node#id tree)
	  with (Failure "addnode") -> tree
	in
	(Route.nth_hop !routeref (o2v hopno)).Route.info <- Some newtree
    | `RREP | `RADV | `RERR -> () (* ignore RREP/RADV/RERR*)
    | `NONE -> Log.log#log_error 
	(lazy 
	  "Od_hooks.od_route_pktin_mhook: unexpected packet type not aodv or grep")
)

let od_route_pktout_mhook routeref l2pkt node = (
  
  let l3pkt = (L2pkt.l3pkt l2pkt) in
  let l3src = L3pkt.l3src l3pkt 
  and l2src = (L2pkt.l2src l2pkt) in
  
  if (l2src <> node#id) then failwith "Od_hooks.od_route_pktout_mhook";
  
  match aodv_grep_flags l3pkt with
    | `DATA ->
	(Log.log)#log_info (lazy (Printf.sprintf "Leaving node %d" node#id));	
	routeref := Route.add_hop !routeref {
	  Route.hop=node#id;
	  Route.info=None
	}
	  
    | `RREQ when (l3src = l2src) ->	(* RREQ leaving initiator *)
	begin	
	  match Route.length !routeref with
	      (* Add hop if this node is not yet on the route 
		 (normally because either 
		 a. first hop had no rtentry, and so has not yet sent a DATA, or
		 b. intermediate hop with no rtentry)   *) 
	    | 0 ->
	      routeref := Route.add_hop !routeref {
		Route.hop=node#id;
		Route.info=Some (Flood.create l3src)
	      }
	    | _ when ((Route.last_hop !routeref).Route.hop <> node#id) ->
	      routeref := Route.add_hop !routeref {
		Route.hop=node#id;
		Route.info=Some (Flood.create l3src)
	      }
	  | _ when ((Route.last_hop !routeref).Route.hop = node#id) ->
	      (* If RREQ initiator is already current last hop, this
		 means that it either just sent a DATA (which failed, hence
		 this RREQ), or that this is a new RREQ (because previous
		 failed) with increase ttl. In either case, we should create a
		 new flood structure, discarding the old one (if any). *)
	      (Route.last_hop !routeref).Route.info <- Some (Flood.create l3src);
	  | _ -> raise (Misc.Impossible_Case "Od_hooks.od_route_pktout_mhook");
	end	      

    | `RREQ -> () (* RREQ at relay node *)
    | `RREP | `RADV | `RERR -> () (* ignore RREP/RADV/RERR*)
    | `NONE -> Log.log#log_error 
	(lazy 
	  "Od_hooks.od_route_pktout_mhook: unexpected packet type not aodv or grep")
)

