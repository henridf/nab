type coordi_t = int array
type coordf_t = float array

val x : 'a array -> 'a
val y : 'a array -> 'a

val (+++) : coordi_t -> coordi_t -> coordi_t
val (---) : coordi_t -> coordi_t -> coordi_t
val (///) : coordi_t -> int -> coordi_t

val (+++.) : coordf_t -> coordf_t -> coordf_t
val (---.) : coordf_t -> coordf_t -> coordf_t
val (///.) : coordf_t -> float -> coordf_t

val coordi2pair : coordi_t -> (int * int)
val coordf2pair : coordf_t -> (float * float)

val coord_i2f : coordi_t -> coordf_t
val coord_f2i : coordf_t -> coordi_t
val coord_round : coordf_t -> coordf_t

val normi_sq : coordi_t -> int
val normi : coordi_t -> float
val disti_sq : coordi_t -> coordi_t -> int
 
val norm_sq : coordf_t -> float
val norm : coordf_t -> float
val dist_sq : coordf_t -> coordf_t -> float

val normalize : coordf_t -> coordf_t
