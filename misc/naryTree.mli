type 'a t = Empty | Node of 'a * 'a t list

(* val map ('a -> 'b) -> 'a naryTree -> 'b naryTree *)

val belongs : 'a -> 'a t -> bool
val height :  'a t -> int



