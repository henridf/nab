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
*)

class nullmac :
  ?stack:int ->
  float ->
  #Simplenode.simplenode ->
  object
    inherit Mac.t
    method other_stats : unit
  end

val macs : ?stack:int -> unit -> (Common.nodeid_t, nullmac) Hashtbl.t 

val mac :  ?stack:int -> Common.nodeid_t -> nullmac
