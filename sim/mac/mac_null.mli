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
  A null MAC layer, where there are no collisions, no losses. 
  A null MAC accepts a packet for reception or transmission even when it is
  already receiving or sending.
  Only propagation and transmission delay are applied.
  No queuing/buffering necessary.

  The [pkts_RX] and [pkts_TX] fields in the {!Mac.basic_stats} record returned
  by the method [basic_stats] count all packets (there are no control packets
  in a null MAC anyway).
*)

class nullmac :
  ?stack:int ->
  bps:float ->
  #Simplenode.simplenode ->
  object
    inherit Mac.t

    method other_stats : unit
      (** A nullmac does not keep any additional stats beyond
	{!Mac.basic_stats}. *)
  end

val macs : ?stack:int -> unit -> (Common.nodeid_t, nullmac) Hashtbl.t 
  (** Returns a hashtbl, indexed by {!Common.nodeid_t}, of all the mac objects
    for stack [stack] (stack defaults to 0 if argument not provided). 
    If there are no macs on this stack, returns an empty hashtbl.
*)

val mac :  ?stack:int -> Common.nodeid_t -> nullmac
  (** Returns the nullmac mac object for given node on given stack.
    @raise Not_found if there is no nullmac on this stack/node pair.
  *)

