(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* hooks:

   pktin, pktout, mhooks.
   mob mhook

*)

(* coordinates: gtk must convert between mws coordinates (meters) and screen coordinates.
   All other parts of mws are screen-unaware and use mws coordinates *)

val node_moved : 
  Coord.coordf_t -> 
  Node.node_t -> 
  unit


val attach_mob_hooks :
  unit -> 
  unit


