(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Mobility generators and related stuff. 
  @author Henri Dubois-Ferriere.
*)



(** The base class for all classes implementing a mobility process *)
class virtual mobility :
  #Simplenode.simplenode ->
  ?gran:float ->
  unit ->
  object

    method objdescr : string
      
    method set_speed_mps : float -> unit
      (** Set the speed in meters/sec. *)
      
    method  start : unit
      (** Stop movement. Idempotent. *)

    method stop : unit
      (** Start movement. Idempotent. *)

    method virtual getnewpos : gran:float -> Coord.coordf_t
      (** This method is called in order to get the next position for the node
	associated with this mobility object.
	The frequency at which it is called depends on the speed of the node.
	parameter {i gran} is the required {i granularity}, or {i distance} of the
	movement, expressed in meters. This allows tuning the 'resolution' of
	movement steps. This parameter can be ignored by processes such as a
	discrete random walk *)
	
  end

(** Uniform waypoint mobility class: waypoints are chosen at random uniformly
  over surface. *)
class uniwaypoint :
  #Simplenode.simplenode ->
  ?gran:float ->
  unit ->
  object
    inherit mobility
    method getnewpos : gran:float -> Coord.coordf_t
  end

(** Border waypoint mobility class: waypoints are chosen only on the borders. *)
class borderwaypoint :
  #Simplenode.simplenode ->
  ?gran:float ->
  unit ->
  object
    inherit mobility
    method getnewpos : gran:float -> Coord.coordf_t
  end

(** Waypoint over EPFL mobility class. *)
class epfl_waypoint :
  #Simplenode.simplenode ->
  unit ->
  object
    inherit mobility
    method getnewpos : gran:float -> Coord.coordf_t
  end
  
  
  
(** Discrete Random Walk, moves by 1.0 in one of 4 cardinal directions at each
  step *)
class discreteRandomWalk :
  #Simplenode.simplenode ->
  unit ->
  object
    inherit mobility
    method getnewpos : gran:float -> Coord.coordf_t
  end



(**/**)
(* xxx/hack copied from gui_hooks b/c otherwise makefile problems in using
   Gui_hooks.* from here *)
val x_mtr_to_pix : float -> int

val pos_pix_to_mtr : Coord.coordi_t -> Coord.coordf_t
val pos_mtr_to_pix : Coord.coordf_t -> Coord.coordi_t
