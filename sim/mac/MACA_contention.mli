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


(** A MAC layer with a MACA backend and a contention frontend. 

  @author Henri Dubois-Ferriere.
*)

class maca_contentionmac :
  ?stack:int ->
  bps:float ->
  #Node.node ->
  object
    inherit Mac.t
    method other_stats : Contention_frontend.stats * MACA_backend.stats
end




val macs : ?stack:int -> unit -> (Common.nodeid_t, maca_contentionmac) Hashtbl.t 
  (** Returns a hashtbl, indexed by {!Common.nodeid_t}, of all the mac objects
    for stack [stack] (stack defaults to 0 if argument not provided). 
    If there are no macs on this stack, returns an empty hashtbl.
  *)

val mac :  ?stack:int -> Common.nodeid_t -> maca_contentionmac
  (** Returns the maca_mac mac object for given node on given stack.
    @raise Not_found if there is no maca_mac on this stack/node pair.
  *)



