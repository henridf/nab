(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)





(** Interface into discrete event schedulers. 
  @author Henri Dubois-Ferriere.
*)



type  sched_time_t = 
  | ASAP (** Schedule as soon as possible. *)
  | ALAP (** Schedule as late as possible. *)
  | Time of Time.time_t  (** Schedule at given time. *)

type handle = int
    (** A handle is an id for events registered with the scheduler which can
      be used to cancel them before they happen. 
      A handle is always different than 0. *)


exception AlreadyCancelled
exception AlreadyExecuted
exception InvalidHandle


(** The virtual scheduler class. Contains all logic which is independent of
  the underlying data structure used to keep events.
  All that is left for a concrete class is to implement two virtual methods,
  allowing to insert and remove events into whatever data structure the
  concrete class uses. *)
class type t = 
object

  inherit Log.inheritable_loggable 

  method sched_at : f:(unit -> unit) -> t:sched_time_t -> unit
    (** Schedule the event [f] to run at time [t]. *)

  method sched_in : f:(unit -> unit) -> t:float -> unit
    (** Schedule the event [f] to run [t] simulated seconds from now. *)

  method sched_at_handle : f:(unit -> unit) -> t:sched_time_t -> handle
    (** Same as [sched_at], except that a handle to the event is returned,
      which may be used subsequently to cancel it. *)

  method sched_in_handle : f:(unit -> unit) -> t:float -> handle
    (** Same as [sched_in], except that a handle to the event is returned,
      which may be used subsequently to cancel it. *)

  method cancel : handle -> unit
    (** Cancel an event. 
      @raise AlreadyCancelled if this event has already been cancelled.
      @raise AlreadyExecuted if this event has already taken place.
      @raise InvalidHandle if this handle was never issued.
    *)

  method stop_at : t:sched_time_t -> unit
    (** Schedule a "stop" event at time [t]. A stop event interrupts the
      scheduler, and control returns to the point where [run] (or [run_for],
      or [run_until], see below) was called.
      Future scheduled events remain in the scheduler. *)

  method stop_in : t:float -> unit
    (** Schedule a "stop" event to run [t] simulated seconds from now. *)

  method run : unit -> unit
    (** Start running the event loop. Events are processed until the event
      queue is empty, or a "stop" event is encountered. *)

  method run_for : duration:float -> unit
    (** Start running the event loop. Events are processed until [duration]
      simulated seconds have elapsed, the event queue is empty, or a "stop"
      event is encountered. *)

  method run_until : continue:(unit -> bool) -> unit
    (** Start running the event loop. Events are processed until [continue()]
      evaluates to [false], the event queue is empty, or a "stop"
      event is encountered. *)

end

