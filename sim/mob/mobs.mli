(** Some simple mobility processes.
  @author Henri Dubois-Ferriere.
*)



(** Uniform waypoint mobility class: waypoints are chosen at random uniformly
  over surface. *)
class uniwaypoint : 
  #Simplenode.simplenode ->  
  ?gran:float ->
  unit ->
  Mob.t

(** Border waypoint mobility class: waypoints are chosen only on the borders. *)
class borderwaypoint :
  #Simplenode.simplenode ->
  ?gran:float ->
  unit ->
  Mob.t

(** Waypoint over EPFL mobility class. *)
class epfl_waypoint :
  #Simplenode.simplenode ->
  unit ->
  Mob.t
  
  
(** Billiard mobility model: 
  Pick a uniform random direction, advance in that direction. A new random
  direction is selected every T seconds, where T is exponentially distributed;
  E(T) is {!Params.x_size}[ * speed ], ie the time it takes to traverse the
  network.
  Boundaries are reflecting, ie a node bounces off as off of a mirror.
*)
class billiard : 
  #Simplenode.simplenode ->
  ?gran:float ->
  unit ->
  Mob.t

(** Discrete Random Walk, moves by 1.0 in one of 4 cardinal directions at each
  step *)
class discreteRandomWalk :
  #Simplenode.simplenode ->
  unit ->
  Mob.t
