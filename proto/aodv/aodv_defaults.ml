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
  AODV defaults and constants. From RFC (xxx name).
  @author Henri Dubois-Ferriere.
*)

open Misc

let aodv_ACTIVE_ROUTE_TIMEOUT =   3000 (* Milliseconds *)
let aodv_ALLOWED_HELLO_LOSS =     2
let aodv_K =                      5
let aodv_HELLO_INTERVAL =         1000 (* Milliseconds*)
let aodv_DELETE_PERIOD =          aodv_K * (max aodv_ACTIVE_ROUTE_TIMEOUT aodv_HELLO_INTERVAL)
let aodv_LOCAL_ADD_TTL =          2
let aodv_MY_ROUTE_TIMEOUT =       2 * aodv_ACTIVE_ROUTE_TIMEOUT
let aodv_NET_DIAMETER =           35
let aodv_MAX_REPAIR_TTL =         int (0.3 *. (float aodv_NET_DIAMETER))
let aodv_NODE_TRAVERSAL_TIME =    40 (* Milliseconds *)
let aodv_NEXT_HOP_WAIT =          aodv_NODE_TRAVERSAL_TIME + 10
let aodv_NET_TRAVERSAL_TIME =     2 * aodv_NODE_TRAVERSAL_TIME * aodv_NET_DIAMETER
let aodv_PATH_DISCOVERY_TIME =    2 * aodv_NET_TRAVERSAL_TIME
let aodv_RERR_RATELIMIT =         10
let aodv_RREQ_RETRIES =           2
let aodv_BLACKLIST_TIMEOUT =      aodv_RREQ_RETRIES * aodv_NET_TRAVERSAL_TIME
let aodv_RREQ_RATELIMIT =         10
let aodv_TIMEOUT_BUFFER =         2
let aodv_TTL_START =              1
let aodv_TTL_INCREMENT =          2
let aodv_TTL_THRESHOLD =          7

let aodv_RING_TRAVERSAL_TIME ttl_value =  
  2 * aodv_NODE_TRAVERSAL_TIME * (ttl_value + aodv_TIMEOUT_BUFFER)
