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







(** EASE packet types and manipulators.
  @author Henri Dubois-Ferriere.
*)


type t = {
  mutable enc_age : Time.time_t;
  mutable anchor_pos : Coord.coordf_t;
  mutable search_dist : float;
}

val make_ler_hdr : 
  enc_age:Time.time_t -> 
  anchor_pos:Coord.coordf_t -> 
  t

val anchor : t ->  Coord.coordf_t
val enc_age : t ->  Time.time_t
val search_dist : t ->  float

val set_search_dist : t -> float -> unit
val set_enc_age : t -> Time.time_t -> unit
val set_anchor_pos : t -> Coord.coordf_t -> unit

val clone : t -> t
val size : t -> int
