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
