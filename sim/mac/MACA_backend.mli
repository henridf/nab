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
  MACA (Multiple Access, Collision Avoidance) Mac layer backend.

  This implementation is a direct transcription of Appendix A in the paper 
  "MACAW: A Media Access Protocol for Wireless LANs", by V. Bharghavan,
  A. Demers, S. Shenker, and L. Zhang.

  The [pkts_RX] and [pkts_TX] fields in the {!Mac.basic_stats} record returned
  by the method [basic_stats] count only data packets (counters for CTS and
  RTS packets are maintained in {!MACA_backend.stats}).

  @author Henri Dubois-Ferriere.
*)

val out_queue_size : int 
  (** Size of outgoing packet queue. *)

type stats = 
    {colls:Contention_frontend.stats;
    cts_RX:int;
    cts_TX:int;
    rts_RX:int;
    rts_TX:int;
    drops:int
    }

(** A MAC backend implementing the MACA protocol. This is not a complete MAC,
  it does not have a frontend. Complete MACA implementations can be found in
  {!MACA_simple.maca_mac} (MACA with null frontend) or
  {!MACA_contention.maca_contentionmac} (MACA with contention frontend). *)
class virtual maca_backend :
  ?stack:int ->
  bps:float ->
  #Node.node ->
  object
    inherit Log.virtual_loggable 
    inherit [stats] Mac.backend_t
  end



(** {1 Functions for manipulating {!MACA_backend.stats} .} *) 

val string_of_ostats_colls : stats -> string
  (** Return a string representation of the collision stats in a
    {!MACA_backend.stats}. *)

val string_of_ostats_pkts : stats -> string
  (** Return a string representation of the packet counter stats in a
    {!MACA_backend.stats}. *)

val string_of_ostats_colls : stats -> string
  (** Return a string representation of a collision stats in a
    {!MACA_backend.stats}. *)

val add_ostats : stats -> stats -> stats
  (** Add two {!MACA_backend.stats}, field by field. *)

val zero_ostats : unit -> stats
  (** Return a {!MACA_backend.stats} with all values initialized to 0. *)
