(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Coord



type nodeid_t = int

type time_t = float

type enc_t = {
  mutable t: time_t; 
  mutable p: coordf_t
}

val enc : time:time_t -> place:Coord.coordf_t -> enc_t
val enc_age : enc_t -> time_t

val set_time : time_t -> unit
val get_time : unit -> time_t
val time: unit -> time_t
