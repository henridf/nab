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







(** Core simulator parameters.  
  @author Henri Dubois-Ferriere.
*)

(* These parameters have not (yet?) been examined to figure out if they  *)
(* really need to be globally acessible.                                 *)

val nodes : int Param.t
  (** The number of nodes in the simulation. *)

val x_size : float Param.t
  (** The X (meters) size  of the simulation area. *)

val y_size : float Param.t
  (** The Y (meters) size  of the simulation area. *)

val x_pix_size : int Param.t
  (** The X (pixels) size  of the simulation area, when using a GUI. *)

val y_pix_size : int Param.t
  (** The Y (pixels) size  of the simulation area, when using a GUI. *)

val radiorange : float Param.t
  (** Radio range (meters) of nodes *)

val mob_gran : float Param.t
  (** The granularity at which nodes move. 
    See {!Mob.t}.*)
    
(*
val world : string Param.t
  (** The type of world representation (taurus vs reflecting, lazy vs greedy, epfl)
    that is to be used in this simulation. @see 'worldt.ml' for details. *)
*)
val log_level : string Param.t
  (** Logging level. *)

val mac : string Param.t
  (** Mac layer used. *)
