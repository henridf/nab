(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)







(** Base class for mobility processes following the {!Mob.t} signature.
  @author Henri Dubois-Ferriere.
*)



(** The base class for all classes implementing a mobility process.
  To implement a new mobility process, one must inherit from this class and
  implement the virtual method [getnewpos].
*)
class virtual mobility :
  #Simplenode.simplenode ->
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
    
end

