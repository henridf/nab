(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



(* Nodeid_t Route.t is not possible (as in bler) b/c anchors cannot be
   represented as nodes *)
val ease_route_pktin_mhook : 
  Coord.coordf_t Route.ease_route_t ref ->
  L2pkt.t ->
  Gpsnode.gpsnode -> 
  unit

val ease_route_pktout_mhook : 
  Coord.coordf_t Route.ease_route_t ref ->
  L2pkt.t ->
  Gpsnode.gpsnode -> 
  unit

val grep_route_pktin_mhook : 
  Common.nodeid_t Route.grep_route_t ref ->
  L2pkt.t ->
  Simplenode.simplenode -> 
  unit

val grep_route_pktout_mhook : 
  Common.nodeid_t Route.grep_route_t ref ->
  L2pkt.t ->
  Simplenode.simplenode -> 
  unit

val route_done : bool ref


