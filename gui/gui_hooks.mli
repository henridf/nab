(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* hooks:

   pktin, pktout, mhooks.
   mob mhook

*)

val init : unit -> unit

(* coordinates: gtk must convert between mws coordinates (meters) and screen coordinates.
   All other parts of mws are screen-unaware and use mws coordinates *)

val x_mtr_to_pix : float -> int

val pos_pix_to_mtr : Coord.coordi_t -> Coord.coordf_t
val pos_mtr_to_pix : Coord.coordf_t -> Coord.coordi_t


(* mws => gui *)

val node_moved : 
  Coord.coordf_t -> 
  Simplenode.simplenode -> 
  unit

val attach_mob_hooks :
  unit -> 
  unit

(* Nodeid_t Route.t is not possible (as in bler) b/c anchors cannot be
   represented as nodes *)
val ease_route_pktin_mhook : 
  Coord.coordf_t Route.t ref ->
  L2pkt.l2packet_t ->
  Gpsnode.gpsnode -> 
  unit

val ease_route_pktout_mhook : 
  Coord.coordf_t Route.t ref ->
  L2pkt.l2packet_t ->
  Gpsnode.gpsnode -> 
  unit

val route_done : bool ref

val mtr_2_pix_route :
  Coord.coordf_t Route.t  ->
  Coord.coordi_t Route.t  

(* gui => mws *)
val closest_node_at :
  Coord.coordi_t ->
    Common.nodeid_t
