(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)



(** Note that there can only be one world object *)

val set_lazy_world : Worldt.lazy_world_t -> unit
  (** Sets the global lazy world object. *)

val set_greedy_world : Worldt.greedy_world_t -> unit
  (** Sets the global lazy world object. *)

val w : unit -> Worldt.lazy_world_t
  (** Returns the global lazy_world object, if one has been set. 
    Otherwise, returns the global greedy_world object, coerced to a
    lazy_world, if a greedy_world has been set. *)

val gw : unit -> Worldt.greedy_world_t
  (** Returns the global greedy_world object, if one has been set. *)

(**
val strset_world : string -> unit
  (** Set the desired world type via a string (for example provided as cmdline argument). *)
*)
