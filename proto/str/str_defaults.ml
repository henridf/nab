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

let str_ACTIVE_ROUTE_TIMEOUT =   5. (* Seconds *)
let str_ALLOWED_HELLO_LOSS =     2.
let str_K =                      5
let str_HELLO_INTERVAL =         5. (* Seconds *)
let str_DELETE_PERIOD =          (float str_K) *. (max str_ACTIVE_ROUTE_TIMEOUT str_HELLO_INTERVAL)
let str_LOCAL_ADD_TTL =          2
let str_MY_ROUTE_TIMEOUT =       2. *. str_ACTIVE_ROUTE_TIMEOUT
let str_NET_DIAMETER =           75
let str_MAX_REPAIR_TTL =         int (0.3 *. (float str_NET_DIAMETER))
let str_NODE_TRAVERSAL_TIME =    0.04 (* 40 ms *)
let str_NEXT_HOP_WAIT =          str_NODE_TRAVERSAL_TIME +. 10.
let str_NET_TRAVERSAL_TIME =     2. *. str_NODE_TRAVERSAL_TIME *. (float str_NET_DIAMETER)
let str_PATH_DISCOVERY_TIME =    2. *. str_NET_TRAVERSAL_TIME
let str_RERR_RATELIMIT =         10
let str_RREQ_RETRIES =           2
let str_BLACKLIST_TIMEOUT =      (float str_RREQ_RETRIES) *. str_NET_TRAVERSAL_TIME
let str_RREQ_RATELIMIT =         10
let str_TIMEOUT_BUFFER =         2
let str_TTL_START =              2
let str_TTL_INCREMENT =          2
let str_TTL_MULT =               2
let str_TTL_THRESHOLD =          15

let str_RING_TRAVERSAL_TIME ttl_value =  
  2. *. str_NODE_TRAVERSAL_TIME *. (float (ttl_value + str_TIMEOUT_BUFFER))


(* Configuration parameters not defined in RFC *)
let str_PKTQUEUE_SIZE = 50 


