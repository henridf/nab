



(** Mobility generators and related stuff. 
  @author Henri Dubois-Ferriere.
*)



(** The base class for all classes implementing a mobility process *)
class type t =
object
  
  inherit Log.inheritable_loggable 
    
  method set_speed_mps : float -> unit
    (** Set the speed in meters/sec. *)
    
  method  start : unit
    (** Stop movement. Idempotent. *)

  method stop : unit
    (** Start movement. Idempotent. *)

  method getnewpos : gran:float -> Coord.coordf_t
    (** This method is called in order to get the next position for the node
      associated with this mobility object.
      The frequency at which it is called depends on the speed of the node.
      parameter {i gran} is the required {i granularity}, or {i distance} of the
      movement, expressed in meters. This allows tuning the 'resolution' of
      movement steps. This parameter can be ignored by processes such as a
      discrete random walk.
      Be careful: If high speed and low granularity, then load the scheduler
      will be given "large" numbers of small movement events.
    *)
    
end

