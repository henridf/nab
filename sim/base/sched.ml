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

(* $Header$ *)







open Misc
open Scheduler
open Printf

type handler_t = Handler of (unit -> unit) | Stop 

(* An event is composed of 
   - an event handler 
   - the time at which the event is  scheduled to occur
   - a handle which may be used to cancel the event (this handle is 0 for
   events that are non-cancellable, ie events not scheduled with
   sched_in_handle or sched_at_handle - we  assume that a vast majority of
   events are non-cancellable.
*)
type event_t = {
  handle:int;
  handler:handler_t;
  time:Time.time_t;}



let last_time = ref (Unix.gettimeofday())
let next_check_point = ref 10.
let check_increment = !next_check_point





let compare ev1 ev2 = ev1.time < ev2.time

(* The virtual scheduler class. Contains all logic which is independent of
  the underlying data structure used to keep events.
  All that is left for a concrete class is to implement two virtual methods,
  allowing to insert and remove events into whatever data structure the
  concrete class uses. *)  
class virtual sched  = 

object(s)

  inherit Log.inheritable_loggable

  (* just to keep some stats *)
  val mutable cancels = 0
  val mutable cancellables = 0
  val mutable scheduleds = 0

  val mutable next_handle_to_give = 1
  val handles = Hashtbl.create 64
    (* We keep track of handles that have been assigned on sched_at_handle and
       sched_in_handle calls. We keep them as (handle, bool) bindings in a
       hashtable, where bool is true if the event has been cancelled, false
       otherwise.*)
       
  initializer 
    Pervasives.at_exit (fun () -> s#dump_stats)


  method virtual private next_event : event_t option 

  method virtual private sched_event_at : ev:event_t -> unit

  method stop_at ~t =  s#sched_handler_at  ~handler:Stop ~t ()
    
  method stop_in ~t =  s#stop_at  ~t:(Time (t +. Time.get_time()))

  method private sched_handler_at ?(handle=0) ~handler ~t () = (
    scheduleds <- scheduleds + 1;
    match t with 
      | ASAP -> 
	  s#sched_event_at {handle=handle; handler=handler; time=Time.get_time()};
      | ALAP -> raise (Failure "sched#sched_handler_at: ALAP not implemented\n")
      | Time t -> (
	  if (t < Time.get_time()) then 
	    raise 
	      (Failure 
		("sched#sched_handler_at: attempt to schedule an event in the"
		^
		(Printf.sprintf "past at time %f, current time is %f"
		  t (Time.time())))
	      );
	  s#sched_event_at {handle=handle; handler=handler; time=t};

(*	  s#log_debug (lazy (sprintf "scheduling event at %.8f" t));
	  if ((Time.get_time()) > !next_check_point) then (
	    let r = ref (Unix.gettimeofday()) in
	    s#log_notice (lazy (sprintf "%f runtime (Time: %2f)" 
	      (!r -. !last_time) (Time.get_time())));
	    next_check_point := (!next_check_point +. check_increment);
	    last_time := !r
	  )
*)

	);
  )

  method sched_at ~f ~t = 
    s#sched_handler_at ~handler:(Handler f) ~t ()
    
  method sched_at_handle ~f ~t = (
    cancellables <- cancellables + 1;
    let handle = next_handle_to_give in
    s#sched_handler_at ~handle ~handler:(Handler f) ~t ();
    next_handle_to_give <- next_handle_to_give + 1;


    if next_handle_to_give = max_int then (
      (* if events scheduled with sched_{in,at}_handle are rare, then this
	 overflow is unlikely to ever happen.
	 should deal with this properly anyway (see also comment in method
	 cancel).*)
      s#log_always (lazy "Sched.sched_at_handle: overflow");
      exit (-1);
    );
    
    (* given above check , this should be impossible, but impossible ain't 
       real .. *)
    assert (next_handle_to_give <> 0);

    Hashtbl.add handles handle false;
    handle
  )

  method sched_in ~f ~t = 
    s#sched_at ~f ~t:(Time (t +. Time.get_time())) 

  method sched_in_handle ~f ~t = 
    s#sched_at_handle  ~f ~t:(Time (t +. Time.get_time()))

      
  method cancel (h:int) = (
    if h >= next_handle_to_give then
    (* assumes that handle never wraps *)
      raise Scheduler.InvalidHandle;

    if not (Hashtbl.mem handles h) then 
      raise Scheduler.AlreadyExecuted;

    if Hashtbl.find handles h then
      raise Scheduler.AlreadyCancelled;

    cancels <- cancels + 1;
    Hashtbl.replace handles h true
  )

  method private cancelled h = 
    assert (Hashtbl.mem handles h);
    let c = (Hashtbl.find handles h) in 
    Hashtbl.remove handles h;
    c


  method run_until ~continue = (
    try 
      while (true) do 
	match s#next_event with
	  | None -> raise Misc.Break
	  | Some ev -> 
	      begin
		Time.set_time (ev.time);
		match ev.handler with
		  | Stop -> 
		      raise Misc.Break
		  | Handler h -> (
		      if not (ev.handle <> 0 && s#cancelled ev.handle) then
			h();
		      if (not (continue())) then 
			raise Misc.Break;
		    )
	      end;
      done;
      
    with
      | Misc.Break -> ()
      | o -> raise o
    )

  method run_for ~duration = 
    let end_time = (Time.get_time()) +. duration in
    let continue  = (fun () -> (Time.get_time()) < end_time) in
    s#run_until ~continue;
    (* if for example you run_for 200 secs, and there are no more events after
       10 secs, you still expect to be at time 200.*)
    if (Time.get_time()) < end_time then
      Time.set_time end_time

  method run() = 
    s#run_until ~continue:(fun () -> true)


  method private dump_stats = 
    s#log_always (lazy (Printf.sprintf "%d events scheduled." scheduleds));
    s#log_always (lazy (Printf.sprintf "%d cancellable events scheduled." cancellables));
    s#log_always (lazy (Printf.sprintf "%d events cancelled." cancels));

end

class schedList : Scheduler.t = 
object(s)
  inherit sched 

  initializer 
    s#set_objdescr "/sched/list"

  val ll = Linkedlist.create()

  method private next_event =  Linkedlist.pophead ~ll:ll 
    
  method private sched_event_at ~ev = 
    Linkedlist.insert ~ll:ll ~v:ev ~compare:compare

end



module Compare =
struct
  type t = event_t
  let compare ev1 ev2 = 
    if ev1.time = ev2.time then 0 
    else if ev1.time > ev2.time  then -1 else 1
      
end
module EventHeap = Heap.Imperative (Compare)

class  schedHeap : Scheduler.t = 
object(s)
  inherit sched

  initializer 
    s#set_objdescr "/sched/heap"

  val heap = EventHeap.create 2048

  method private next_event =  
    if EventHeap.is_empty heap then 
      None 
    else 
      Some (EventHeap.pop_maximum  heap)
    
  method private sched_event_at ~ev = 
    EventHeap.add  heap ev

end

let (sched_:Scheduler.t option ref) = ref None 
let s () = o2v !sched_
let set_sched s = sched_ := Some s

let () = 
  set_sched (new schedHeap)

(*
  Test code, wrote this when i added cancellability (so focused on testing
  that, not the rest).

exception Test_Failure
let events_called = ref 0
let cancel_handles = ref []
let wont_cancel_this_one = ref 0
let test = 
  Log.log#log_always (lazy "starting scheduler tests");
  let s = (new schedHeap) in

  let n = 100000 in
  let ok_fun () = incr events_called in
  let cancelled_fun () = raise Test_Failure in

  let cancel_now_or_later h = (
    match (Random.int 3) with
      | 1 -> s#cancel h
      | _ -> cancel_handles := h :: !cancel_handles 
  ) in

  for i = 1 to n do
    let t = Random.float 1000. in
    
    begin match (Random.int 50) with 
      | 1 -> 
	  
	  begin match (Random.int 5) with
	    | 1 -> let h = s#sched_in_handle ~f:cancelled_fun ~t in
	      cancel_now_or_later h
	    | _ -> wont_cancel_this_one := (s#sched_in_handle ~f:ok_fun ~t)
	  end
      | _ -> s#sched_in ~f:ok_fun ~t
    end;
  done;
	  
  List.iter (fun h -> s#cancel h) !cancel_handles;
  
  begin 
    try s#cancel (List.hd !cancel_handles) 
    with Scheduler.AlreadyCancelled -> ();
  end;

  s#run();
  
  try s#cancel max_int with Scheduler.InvalidHandle -> ();
    try s#cancel (List.hd !cancel_handles) with Scheduler.AlreadyExecuted -> ();
      try s#cancel !wont_cancel_this_one with Scheduler.AlreadyExecuted -> ()
	

*)
