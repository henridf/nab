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
 
let find_last_flood route rreq_orig = 
  let n = ref None in
  for i = (List.length route) - 1 downto 0 do
    if (List.nth route i).Route.info <> None && 
      NaryTree.root (o2v (Route.nth_hop route i).Route.info) = rreq_orig then
	if !n = None then
	  n := Some i
  done;
  !n

type u = [ `RERR | Grep_pkt.grep_flags_t | `NONE]

let pkt_type l3pkt = 
  match L3pkt.l3hdr_ext l3pkt with 
    | `AODV_HDR h -> 
	begin match h with 
	  | Aodv_pkt.DATA -> `DATA
	  | Aodv_pkt.RREQ _ -> `RREQ
	  | Aodv_pkt.RREP _ -> `RREP
	  | Aodv_pkt.RERR _ -> `RERR
	  | Aodv_pkt.RREP_ACK -> `NONE end
    | `GREP_HDR h -> ((Grep_pkt.flags h) :> u)
    | `STR_HDR h -> 
	begin match h with 
	  | Str_pkt.DATA _ -> `DATA
	  | Str_pkt.RREQ _ -> `RREQ
	  | Str_pkt.RREP _ -> `RREP
	  | Str_pkt.HELLO -> `RADV end
    | _ -> failwith "Od_hooks.pkt_type : unknown packet type"

let rreq_orig l3pkt = 
  match L3pkt.l3hdr_ext l3pkt with 
    | `AODV_HDR h -> 
	begin match h with 

	  | Aodv_pkt.RREQ r -> r.Aodv_pkt.rreq_orig
	  | Aodv_pkt.RREP _
	  | Aodv_pkt.RERR _
	  | Aodv_pkt.DATA  | Aodv_pkt.RREP_ACK -> failwith "Od_hooks.rreq_orig" end
    | `GREP_HDR h -> failwith "Od_hooks.rreq_orig" 
    | `STR_HDR h -> 
	begin match h with 
	  | Str_pkt.RREQ r -> r.Str_pkt.rreq_orig
	  | Str_pkt.DATA _
	  | Str_pkt.RREP _
	  | Str_pkt.HELLO -> failwith "Od_hooks.rreq_orig" end 
    | _ -> failwith "Od_hooks.pkt_type : unknown packet type"

let rreq_orig l3pkt = 
  match L3pkt.l3hdr_ext l3pkt with 
    | `AODV_HDR h -> begin 
	match h with
	  | Aodv_pkt.RREQ rreq -> rreq.Aodv_pkt.rreq_orig
	  | Aodv_pkt.RREP _ | Aodv_pkt.RERR _ | Aodv_pkt.RREP_ACK 
	  | Aodv_pkt.DATA  -> failwith "Od_hooks.rreq_orig : not a rreq"
      end
    | `GREP_HDR h -> L3pkt.l3src l3pkt
    | `STR_HDR h -> begin 
	match h with
	  | Str_pkt.RREQ rreq -> rreq.Str_pkt.rreq_orig
	  | Str_pkt.RREP _ | Str_pkt.HELLO 
	  | Str_pkt.DATA _ -> failwith "Od_hooks.rreq_orig : not a rreq"
      end
    | _ -> failwith "Od_hooks.rreq_orig : unknown packet type"

let od_route_pktin_mhook routeref l2pkt node = (
  
  let l3pkt = (L2pkt.l3pkt l2pkt) in
  let l3dst, l3src = L3pkt.l3dst l3pkt, L3pkt.l3src l3pkt
  and l2src = (L2pkt.l2src l2pkt) in

  if (l2src = node#id) then failwith "Od_hooks.od_route_pktin_mhook";

  match pkt_type l3pkt with
    | `DATA ->
	Log.log#log_info (lazy (Printf.sprintf "Od_hooks: DATA Arriving at node %d" node#id));	  
	
	if Route.length !routeref = 0 then (*l2src = l3src && (Route.last_hop !routeref).Route.hop <> l3src then *)
	  (* add originator if packet leaving src.
	     2nd test is in case there is a packet loop, the packet goes back
	     through the source, to prevent adding the source twice at this point.
	  *)
	  routeref := Route.add_hop !routeref {
	    Route.hop=l3src;
	    Route.info=None
	  };
	routeref := Route.add_hop !routeref {
	  Route.hop=node#id;
	  Route.info=None
	};
	if  node#id = l3dst then ( (* Packet has arrived. *)
	  incr routes_done;
	)

    | `RREQ  ->
	let rreq_orig = rreq_orig l3pkt in
(*
  assert (Route.length !routeref > 0);
  this assert might fail if there are old rreq packets from a previos route
  lying around*)

	let hopno = find_last_flood !routeref rreq_orig in
	(*
	  assert (if hopno = None then (
	  Log.log#log_error (lazy 
	  (Printf.sprintf "No previous flood for originator %d" rreq_orig));
	  false) else true);
	  
	  if we get a RREQ packet from an originator who is not on the route
	  we're building, assume that it's some leftover form a previous route
	  discovery and don't do anything.
	*)
	if hopno <> None then 
	let tree = o2v (Route.nth_hop !routeref (o2v hopno)).Route.info in
	Log.log#log_debug (lazy 
	  (Printf.sprintf "packet arriving at %d with l2src %d" node#id l2src));	
(*	Log.log#log_info (lazy (NaryTree.sprintf ~f:string_of_int tree));*)

	assert (if not (NaryTree.belongs l2src tree) then (
	  Log.log#log_error (lazy (NaryTree.sprintf ~f:string_of_int tree));
	  false ) else true
	);

	let newtree = 
	  (* A real flood in the network is not a tree, so this node may
	     receive the rreq more than once. we only care for the first
	     time.*)
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
  
  let l3pkt = (L2pkt.l3pkt l2pkt) 
  and l2src = (L2pkt.l2src l2pkt) in
  
  if (l2src <> node#id) then failwith "Od_hooks.od_route_pktout_mhook";
  
  match pkt_type l3pkt with
    | `DATA -> ()
	(* don't add hop when  packet leaving src, because it might leave
	   source twice (when we have a broken next hop. *)
    | `RREQ when (rreq_orig l3pkt = l2src) ->	(* RREQ leaving initiator *)
	begin	
	  Log.log#log_info (lazy (Printf.sprintf "Od_hooks: %d originating RREQ" node#id));	  

	  let orig = rreq_orig l3pkt in
	  match Route.length !routeref with
	      (* Add hop if this node is not yet on the route 
		 (normally because either 
		 a. first hop had no rtentry, and so has not yet sent a DATA, or
		 b. intermediate hop with no rtentry)   *) 
	    | 0 ->
	      routeref := Route.add_hop !routeref {
		Route.hop=node#id;
		Route.info=Some (Flood.create orig)
	      }
	    | _ when ((Route.last_hop !routeref).Route.hop <> node#id) ->
	      routeref := Route.add_hop !routeref {
		Route.hop=node#id;
		Route.info=Some (Flood.create orig)
	      }
	  | _ when ((Route.last_hop !routeref).Route.hop = node#id) ->
	      (* If RREQ initiator is already current last hop, this
		 means that it either just sent a DATA (which failed, hence
		 this RREQ), or that this is a new RREQ (because previous
		 failed) with increase ttl. In either case, we should create a
		 new flood structure, discarding the old one (if any). *)
	      (Route.last_hop !routeref).Route.info <- Some (Flood.create orig);
	  | _ -> raise (Misc.Impossible_Case "Od_hooks.od_route_pktout_mhook");
	end	      

    | `RREQ -> () (* RREQ at relay node *)
    | `RREP | `RADV | `RERR -> () (* ignore RREP/RADV/RERR*)
    | `NONE -> Log.log#log_error 
	(lazy 
	  "Od_hooks.od_route_pktout_mhook: unexpected packet type not aodv or grep")
)

