type 'a coord_t = ('a * 'a)
type coordi_t = int coord_t
type coordf_t = float coord_t
type coordn_t = int array

val xx : 'a coord_t -> 'a
val yy : 'a coord_t -> 'a

val ( +++ ) : coordi_t -> coordi_t -> coordi_t
val ( --- ) : coordi_t -> coordi_t -> coordi_t
val ( *** ) : coordi_t -> int -> coordi_t
val ( /// ) : coordi_t -> int -> coordi_t

val ( +++. ) : coordf_t -> coordf_t -> coordf_t
val ( ---. ) : coordf_t -> coordf_t -> coordf_t
val ( ***. ) : coordf_t -> float -> coordf_t
val ( ///. ) : coordf_t -> float -> coordf_t

val coord_i2f : coordi_t -> coordf_t
val coord_i2n : coordi_t -> coordn_t
val coord_f2i : coordf_t -> coordi_t
val coord_f2n : coordf_t -> coordn_t
val coord_round : coordf_t -> coordf_t
val coord_floor : coordf_t -> coordf_t

val normi_sq : coordi_t -> int
val normi : coordi_t -> float
val disti_sq : coordi_t -> coordi_t -> int
 
val norm_sq : coordf_t -> float
val norm : coordf_t -> float
val dist_sq : coordf_t -> coordf_t -> float

val normalize : coordf_t -> coordf_t

val print : coordi_t -> unit
val sprint : coordi_t -> string
val sprintf : coordf_t -> string
