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
    A MAC layer with a packet send queue that implements an ideal
    rts/cts to avoid collisions.
    Nodes ideally check if a transmission will result into
    packet collision. If so, packets are stored in the queue and
    re-scheduled after a random time.
*)

class ctsmac_q :
  ?stack:int ->
  ?queuesize:int ->
  bps:float ->
  #Node.node ->
object
  inherit Mac.t
    
  method other_stats : Mac_base.mac_cts_stats
  method read_state  : Mac.frontend_state
    
end
  
val macs : ?stack:int -> unit -> (Common.nodeid_t, ctsmac_q) Hashtbl.t 
  (** Returns a hashtbl, indexed by {!Common.nodeid_t}, of all the mac objects
      for stack [stack] (stack defaults to 0 if argument not provided). 
      If there are no macs on this stack, returns an empty hashtbl.
  *)
  
val mac :  ?stack:int -> Common.nodeid_t -> ctsmac_q
  (** Returns the ctsmac_q mac object for given node on given stack.
      @raise Not_found if there is no ctsmac_q on this stack/node pair.
  *)
  
