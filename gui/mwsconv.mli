(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  Conversions/interface between mws and gui. 
  In particular, between mws coordinates (meters) and screen coordinates (pixels).
  @author Henri Dubois-Ferriere.
*)

val init : unit -> unit

val x_mtr_to_pix : float -> int

val pos_pix_to_mtr : Coord.coordi_t -> Coord.coordf_t
val pos_mtr_to_pix : Coord.coordf_t -> Coord.coordi_t

val mtr_2_pix_route :
  Coord.coordf_t Route.t  ->
  Coord.coordi_t Route.t  

val mtr_2_pix_route :
  Coord.coordf_t Route.t  ->
  Coord.coordi_t Route.t  


(* gui => mws *)
val closest_node_at :
  Coord.coordi_t ->
    Common.nodeid_t
