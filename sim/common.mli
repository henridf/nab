(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Coord



type nodeid_t = int

type time_t = float

val set_time : time_t -> unit
val get_time : unit -> time_t
val time: unit -> time_t
