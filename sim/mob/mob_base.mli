(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)




(** Base class for mobility processes following the {!Mob.t} signature.
  @author Henri Dubois-Ferriere.
*)


val mob : Common.nodeid_t -> Mob.t


type base_state = {
  rnd : Random.State.t;
  speed : float;
  moving : bool;
  granularity : float
}

val delete_all_mobs : unit -> unit


(** The base class for all classes implementing a mobility process.
  To implement a new mobility process, one must inherit from this class and
  implement the virtual method [getnewpos].
*)
class virtual ['a] mobility :
  #Node.node ->
  ?gran:float ->
  unit ->
object
  
  inherit Log.inheritable_loggable 
    
  val rnd : Random.State.t
    (** State of RNG used by this mobility process. *)

  val speed_mps : float
    (** Speed in mps of this mobility process. *)

  method set_speed_mps : float -> unit
    (** Set the speed in meters/sec. *)
    
  method speed_mps : float
    (** Return the speed in meters/sec. *)

  method start : unit
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
      discrete random walk.
      Be careful: If high speed and low granularity, then load the scheduler
      will be given "large" numbers of small movement events.
    *)

  method dump_state : unit -> base_state * 'a

  method restore_state : base_state * 'a -> unit

  method virtual private dump_other_state : unit -> 'a
  method virtual private restore_other_state : 'a -> unit

end

class no_other_state_mixin : 
object 
  method private dump_other_state : unit -> unit
  method private restore_other_state : unit -> unit
end

