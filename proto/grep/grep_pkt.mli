(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)







(** GREP packet types and manipulators.
  @author Henri Dubois-Ferriere.
*)


(* L3 STUFF *)
type grep_flags_t = 
 GREP_DATA | GREP_RREQ | GREP_RREP | GREP_RADV

type t = {
  mutable grep_flags : grep_flags_t;
  ssn : int;         (* Source Seqno: All *)
  dsn : int;         (* Destination Seqno: RREQ *)
  mutable shc : int; (* Source hopcount: All *)
  mutable dhc : int; (* Destination hopcount: RREQ *)
  osrc : Common.nodeid_t; (* OBO Source: RREP *)
  osn : int;              (* OBO Seqno: RREP *)
  ohc : int;      (* OBO Hopcount: RREP *)
  rdst : Common.nodeid_t; (* Route request destination : RREQ *)
}

val hdr_size : t -> int

val clone : t -> t

val flags : t -> grep_flags_t
val ssn : t -> int
val shc : t -> int
val dsn : t -> int
val dhc : t -> int
val osrc : t -> Common.nodeid_t
val ohc : t -> int
val osn : t -> int
val rdst : t -> Common.nodeid_t


val incr_shc_pkt : t -> unit
val decr_shc_pkt : t -> unit

val make_grep_hdr :
  ?dhc:int ->
  ?dsn:int ->
  ?osrc:Common.nodeid_t ->
  ?ohc:int ->
  ?osn:int ->
  ?rdst:Common.nodeid_t ->
  flags:grep_flags_t ->
  ssn:int -> 
  shc:int -> 
  unit ->
  t

