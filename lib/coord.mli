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

(* $Header *)







(** Discrete and continuous coordinates.
  @author Henri Dubois-Ferriere.
 *)



type 'a coord_t = ('a * 'a)
    (** The type of 2D coordinates with individual coordinates of type 'a *)
    
type coordi_t = int coord_t
    (** The type of discrete 2D coordinates *)
    
type coordf_t = float coord_t
    (** The type of continuous 2D coordinates *)
    
type coordn_t = int array
    (** The type of discrete, n-dimensional coordinates *)
    
val xx : 'a coord_t -> 'a
  (** Returns the first component of a 2D coordinate *)
  
val yy : 'a coord_t -> 'a
  (** Returns the second component of a 2D coordinate *)
  
  
(** Operations on 2D discrete coordinates *)
  
val ( +++ ) : coordi_t -> coordi_t -> coordi_t
val ( --- ) : coordi_t -> coordi_t -> coordi_t
val ( *** ) : coordi_t -> int -> coordi_t
val ( /// ) : coordi_t -> int -> coordi_t
  
(** Operations on 2D continuous coordinates *)

val ( +++. ) : coordf_t -> coordf_t -> coordf_t
val ( ---. ) : coordf_t -> coordf_t -> coordf_t
val ( ***. ) : coordf_t -> float -> coordf_t
val ( ///. ) : coordf_t -> float -> coordf_t

(** Type conversions *)

val i2f : coordi_t -> coordf_t
val i2n : coordi_t -> coordn_t
val f2i : coordf_t -> coordi_t
val f2n : coordf_t -> coordn_t
val round : coordf_t -> coordf_t
val floor : coordf_t -> coordf_t

(** Norms and distances *)

val normi_sq : coordi_t -> int
val normi : coordi_t -> float
val disti_sq : coordi_t -> coordi_t -> int
 
val norm_sq : coordf_t -> float
val norm : coordf_t -> float
val dist_sq : coordf_t -> coordf_t -> float

val normalize : coordf_t -> coordf_t

(** Printing and string representations *)

val print : coordi_t -> unit
val sprint : coordi_t -> string
val sprintf : coordf_t -> string
