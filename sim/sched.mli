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


type handler_t = Stop | Handler of (unit -> unit)   (** The type of event
						      handlers. *)

and  sched_time_t = | ASAP (** Schedule as soon as possible. *)
		    | ALAP (** Schedule as late as possible. *)
		    | Time of Common.time_t  (** Schedule at given time. *)


type event_t = { handler : handler_t; time : Common.time_t; }   (** The type of events. *)



val set_time : Common.time_t -> unit



class virtual sched :
  object
    val mutable objdescr : string

    method objdescr : string
    method run : unit -> unit
    method run_for : duration:float -> unit
    method run_until : continue:(unit -> bool) -> unit
    method sched_at : f:(unit -> unit) -> t:sched_time_t -> unit
    method sched_in : f:(unit -> unit) -> t:float -> unit
    method stop_at : t:sched_time_t -> unit
    method stop_in : t:float -> unit

    (** Following two virtual methods abstract away insertion and removal of
      events from whatever underlying data structure is needed. *)

    method private virtual next_event : event_t option
    method private virtual sched_event_at : ev:event_t -> unit
  end

class schedList :
  object
    inherit sched
    method private sched_event_at : ev:event_t -> unit
    method private next_event : event_t option
  end


class schedHeap :
  object
    inherit sched
    method private sched_event_at : ev:event_t -> unit
    method private next_event : event_t option
  end
