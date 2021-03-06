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


(** Handling of time.
  @author Henri Dubois-Ferriere.
*)

type time_t = float
type t = time_t


(** The types of timekeeping that are used. *)
type timekeeping_t = 
    SIM (** Simulation time. *)
  | CPU (** Wall-clock time. *)

val set_timekeeping : timekeeping_t -> unit


type abs_or_rel_t = ABS of time_t | REL of time_t
  (** This can be used by methods that take either an absolute or relative. *)

val set_time : time_t -> unit
  (** Set the current time. This should only be used for good reason, for
    example if loading agents with pre-generated Last-Encounter tables, when
    we need to set the clocks to the time at which the warmup ended. *)

val get_time : unit -> time_t
  (** Returns the present simulator time. *)

val time: unit -> time_t
  (** Identical to [get_time]. *)


module Persist : Persist.t



(**/**)
type dtime_t = int
val maintain_discrete_time : unit -> unit
val dtime : unit -> dtime_t
