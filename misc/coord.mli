(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Discrete and continuous coordinates *)



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

val coord_i2f : coordi_t -> coordf_t
val coord_i2n : coordi_t -> coordn_t
val coord_f2i : coordf_t -> coordi_t
val coord_f2n : coordf_t -> coordn_t
val coord_round : coordf_t -> coordf_t
val coord_floor : coordf_t -> coordf_t

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
