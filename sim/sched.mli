(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Discrete event schedulers. 
  @author Henri Dubois-Ferriere.
*)


(** 
  There are currently two schedulers:

  - {!Sched.schedList} is list-based, with O(n) insert time and O(1) pop
  time. Multiple events scheduled to be run at the same time
  (or at ASAP) are executed in FIFO order.

  - {!Sched.schedHeap} is heap-based, with O(logn) insert and O(logn) pop time.
  Multiple events scheduled at the same time (or at ASAP) are executed in
  undefined order.

*)

(** An event handler is either a function of a bla*)
  type handler_t =  Handler of (unit -> unit) | Stop  (** The type of event
						      handlers. *)


(** An event is composed of an event handler ({!Sched.handler_t}) 
  and a time at which the event is scheduled to occur. *)
type event_t = { handler : handler_t; time : Common.time_t; }   

type  sched_time_t = 
  | ASAP (** Schedule as soon as possible. *)
  | ALAP (** Schedule as late as possible. *)
  | Time of Common.time_t  (** Schedule at given time. *)
      


val set_time : Common.time_t -> unit


(** The virtual scheduler class. Contains all logic which is independent of
  the underlying data structure (e.g., heap) used to keep events.
  All that is left for a concrete class is to implement two virtual methods,
  allowing to insert and remove events into whatever data-structure the
  concrete class uses. *)
class virtual sched :
object

  inherit Log.inheritable_loggable 

  method sched_at : f:(unit -> unit) -> t:sched_time_t -> unit
    (** Schedule the event [f] to run at time [t]. *)

  method sched_in : f:(unit -> unit) -> t:float -> unit
    (** Schedule the event [f] to run [t] simulated seconds from now. *)

  method stop_at : t:sched_time_t -> unit
    (** Schedule a "stop" event at time [t]. A stop event interrupts the
      scheduler, and control returns to the point where [run*] was called.
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

  method private virtual next_event : event_t option
    (** Return the next scheduled event, or [None] if no events left in event
      loop. This method is virtual because implementation will depend on data
      structure employed.*)

  method private virtual sched_event_at : ev:event_t -> unit
    (** Insert an event into the scheduler. This method is virtual because
    implementation will depend on data structure employed. *)

end

(**  [schedList] : a list-based, with O(n) insert time and O(1) pop
  time. Multiple events scheduled to be run at the same time
  (or at ASAP) are executed in FIFO order.
*)
class schedList :
object
  inherit sched
  method private sched_event_at : ev:event_t -> unit
  method private next_event : event_t option
end


(**  
  [schedHeap] : A heap-based, with O(logn) insert and O(logn) pop time.
  Multiple events scheduled at the same time (or at ASAP) are executed in
  undefined order.
*)
class schedHeap :
object
  inherit sched
  method private sched_event_at : ev:event_t -> unit
  method private next_event : event_t option
end


val set_sched : sched -> unit
  (** Sets the global scheduler object. *)

val s : unit -> sched
  (** Returns the global scheduler object, if one has been set. *)

