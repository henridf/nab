(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


val init : unit -> unit
val set_mws_scale : x:float -> y:float -> unit
  (* coordinates: gtk must convert between mws coordinates and screen coordinates.
     All other parts of mws are screen-unaware and use mws coordinates *)

val draw_node :  mwspos:Coord.coordf_t -> unit
val undraw_node : mwspos:Coord.coordf_t -> unit


