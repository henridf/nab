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

(** General utils for running LER experiments. *)


val dist_age_arr : ?dst:Common.nodeid_t -> unit -> (float * float) array
  (** [dist_age_arr ~dst ()] returns an array of (dist, age) tuples
    corresponding to the last encounter age and current distance between each
    node and the destination, for those nodes which have met the
    destination. Optional [dst] defaults to 0 if not provided. *)

val dist_age_avg_arr : ?dst:Common.nodeid_t -> npoints:int -> unit -> (float * float) array
  (** averaged (dist, age) array into [npoints] bins. *)

val get_route :
  ?nstacks:int ->
  src:int ->
  dst:Common.nodeid_t ->
  unit -> (Common.nodeid_t, Coord.coordf_t) Ler_route.t array
  (** [get_route ~nstacks src dst] returns an array of size nstacks containing
    routes ({!Ler_route.t}) computed between [src] and [dst] on all [nstacks]
    stacks. *)

    
val proportion_met_nodes : ?stack:int -> unit -> float
  (** Computes the encounter ratio, ie the proportion of source-dst pairs that
    have last-encounter entries for each other.
    This takes into account the value of [ntargets].*)


val dump_gradient : (Common.nodeid_t, Coord.coordf_t) Ler_route.t  -> unit

val dump_3d_gradient : (Common.nodeid_t, Coord.coordf_t) Ler_route.t  -> unit
