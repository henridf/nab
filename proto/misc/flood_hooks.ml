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

let flood_pktin_mhook floodtreeref l2pkt node = (
  
  let l3pkt = (L2pkt.l3pkt l2pkt) in
  let l3dst, l3src = L3pkt.l3dst l3pkt, L3pkt.l3src l3pkt
  and l2src = (L2pkt.l2src l2pkt) in

  if (l2src = node#id) then failwith "Flood_hooks.flood_route_pktin_mhook";

  match L3pkt.l3hdr_ext l3pkt with 

    | `SIMPLE_HDR _  ->
	let orig = L3pkt.l3src l3pkt  in
	let newtree = 
	  (* A real flood in the network is not a tree, so this node may
	     receive the rreq more than once. we only care for the first
	     time.*)
	  try (Flood.addnode  ~parent:l2src ~node:node#id !floodtreeref)
	  with NaryTree.Duplicate_node -> !floodtreeref
	in
	floodtreeref := newtree;
    | _ -> Log.log#log_error (lazy 
	"Flood_hooks.flood_pktin_mhook: unexpected packet type")

)

let flood_pktout_mhook routeref l2pkt node = ()
  
