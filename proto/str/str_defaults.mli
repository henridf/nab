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
  STR defaults and constants. 
  As per draft-ietf-manet-str-13.txt, Feb 2003.

  @author Henri Dubois-Ferriere.
*)

val str_ACTIVE_ROUTE_TIMEOUT : float 
val str_ALLOWED_HELLO_LOSS : float
val str_K : int
val str_HELLO_INTERVAL : float
val str_DELETE_PERIOD : float 
val str_LOCAL_ADD_TTL : int
val str_MY_ROUTE_TIMEOUT : float
val str_NET_DIAMETER : int
val str_MAX_REPAIR_TTL : int
val str_NODE_TRAVERSAL_TIME : float
val str_NEXT_HOP_WAIT : float
val str_NET_TRAVERSAL_TIME : float
val str_PATH_DISCOVERY_TIME : float
val str_RERR_RATELIMIT : int
val str_RREQ_RETRIES : int
val str_BLACKLIST_TIMEOUT : float
val str_RREQ_RATELIMIT : int
val str_TIMEOUT_BUFFER : int
val str_TTL_START : int
val str_TTL_INCREMENT : int
val str_TTL_THRESHOLD : int
val str_RING_TRAVERSAL_TIME : int -> float


(* Configuration parameters not defined in RFC *)
val str_PKTQUEUE_SIZE : int
