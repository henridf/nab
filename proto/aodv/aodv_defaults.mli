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


(** 
  AODV defaults and constants. 
  As per draft-ietf-manet-aodv-13.txt, Feb 2003.

  @author Henri Dubois-Ferriere.
*)

val aodv_ACTIVE_ROUTE_TIMEOUT : float 
val aodv_ALLOWED_HELLO_LOSS : int
val aodv_K : int
val aodv_HELLO_INTERVAL : float
val aodv_DELETE_PERIOD : float 
val aodv_LOCAL_ADD_TTL : int
val aodv_MY_ROUTE_TIMEOUT : float
val aodv_NET_DIAMETER : int
val aodv_MAX_REPAIR_TTL : int
val aodv_NODE_TRAVERSAL_TIME : float
val aodv_NEXT_HOP_WAIT : float
val aodv_NET_TRAVERSAL_TIME : float
val aodv_PATH_DISCOVERY_TIME : float
val aodv_RERR_RATELIMIT : int
val aodv_RREQ_RETRIES : int
val aodv_BLACKLIST_TIMEOUT : float
val aodv_RREQ_RATELIMIT : int
val aodv_TIMEOUT_BUFFER : int
val aodv_TTL_START : int
val aodv_TTL_INCREMENT : int
val aodv_TTL_THRESHOLD : int
val aodv_RING_TRAVERSAL_TIME : int -> float


(* Configuration parameters not defined in RFC *)
val aodv_PKTQUEUE_SIZE : int
