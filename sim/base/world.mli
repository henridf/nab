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


(** Implementations of the {!Worldt.lazy_world_t} and 
  {!Worldt.greedy_world_t} type. *)

val set_lazy_world : Worldt.lazy_world_t -> unit
  (** Sets the global lazy world object. *)

val set_greedy_world : Worldt.greedy_world_t -> unit
  (** Sets the global greedy world object. *)

type world_dim =  One | Two
  (** Dimension of the world that nodes inhabit.*)

type worldtype = 
  | Lazy
  | Greedy
  | Lazy_taurus 
  | Greedy_taurus 
  | Epfl
      (** The type of world that can be used (see sim/interfaces/worldt.ml)*)
      
val world : (worldtype * world_dim) Param.t
  (** Configuration parameter representing the type of world to use. Default
    is Lazy.*)

val w : unit -> Worldt.lazy_world_t
  (** Returns the global lazy_world object, if one has been set. 
    Otherwise, returns the global greedy_world object, coerced to a
    lazy_world, if a greedy_world has been set. *)

val gw : unit -> Worldt.greedy_world_t
  (** Returns the global greedy_world object, if one has been set. *)


(**/**)




