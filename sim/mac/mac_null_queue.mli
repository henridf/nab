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
  A null MAC layer, (no collisions) with a packet send queue.
   It abstracts the wireless case as a graph with point-to-point links
   and transform the problem into a graph with nearest neighbor
   connectivity.
   
   This mac models the following behaviour:
   - nodes can only transmit one packet at the time
   - nodes can receive at the same time from multiple neighbors

*)

class nullmac_q :
  ?stack:int ->
  ?buffer:int ->
  bps:float ->
  #Node.node ->
  object
    inherit Mac.t

    method other_stats : Mac_base.mac_queue_stats

  end

val macs : ?stack:int -> unit -> (Common.nodeid_t, nullmac_q) Hashtbl.t 
  (** Returns a hashtbl, indexed by {!Common.nodeid_t}, of all the mac objects
    for stack [stack] (stack defaults to 0 if argument not provided). 
    If there are no macs on this stack, returns an empty hashtbl.
  *)

val mac :  ?stack:int -> Common.nodeid_t -> nullmac_q
  (** Returns the nullmac_q mac object for given node on given stack.
    @raise Not_found if there is no nullmac_q on this stack/node pair.
  *)

