



(** 
  Helper functions for efficient search of geographic neighbors.
  This is _not_ a 'protocol' search - this is just the back-end simulator 
  infrastructure for figuring out which nodes are neighbors.

  @author Henri Dubois-Ferriere.
*)


(**

  Returns a list of the bottom-left coords of all world squares that are
  touched by the circle.
  worldsize_x : width of world in meters
  worldsize_y : height of world in meters
  boxsize : size of one grid square in meters
*)
val xsect_grid_and_circle : 
  center:Coord.coordf_t -> 
  radius:float -> 
  worldsize_x:float ->
  worldsize_y:float -> 
  boxsize:float ->
  Coord.coordi_t list 

