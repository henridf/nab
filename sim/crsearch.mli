(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  Helper functions for efficient search of geographic neighbors.
  This is _not_ a 'protocol' search - this is just the back-end simulator 
  infrastructure for figuring out which nodes are neighbors.

  @author Henri Dubois-Ferriere.
*)


val xsect_grid_and_circle : 
  center_m:Coord.coordf_t -> 
  radius_m:float -> 
  worldsize_x_m:float ->
  worldsize_y_m:float -> 
  boxsize_m:float ->
  Coord.coordi_t list 
(**

  Returns a list of the bottom-left coords of all world squares that are
  touched by the circle.
  worldsize_x_m : width of world in meters
  worldsize_y_m : height of world in meters
  boxsize_m : size of one grid square in meters
*)
