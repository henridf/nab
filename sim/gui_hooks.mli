(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



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


