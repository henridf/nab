



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




(**  [schedList] : a list-based scheduler, with O(n) insert time and O(1) pop
  time. Multiple events scheduled to be run at the same time
  (or at ASAP) are executed in FIFO order.
*)
class schedList : Scheduler.t

(**  
  [schedHeap] : A heap-based, with O(logn) insert and O(logn) pop time.
  Multiple events scheduled at the same time (or at ASAP) are executed in
  undefined order.
*)
class schedHeap : Scheduler.t

val set_sched : Scheduler.t -> unit
  (** Sets the global scheduler object. *)

val s : unit -> Scheduler.t
  (** Returns the global scheduler object, if one has been set. *)

