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







(** Diffusion packet types and manipulators.
  @author Henri Dubois-Ferriere.
*)


(* L3 STUFF *)
type diff_flags_t = 
    DIFF_DATA | DIFF_RADV

type t = {
  mutable diff_flags : diff_flags_t;
  ssn : int;         (* Source Seqno: All *)
  mutable shc : int; (* Source hopcount: All *)
}

val hdr_size : t -> int

val clone : t -> t

val flags : t -> diff_flags_t
val ssn : t -> int
val shc : t -> int


val incr_shc_pkt : t -> unit
val decr_shc_pkt : t -> unit

val make_diff_hdr :
  flags:diff_flags_t ->
  ssn:int -> 
  shc:int -> 
  t
  
