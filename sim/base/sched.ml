



open Misc
open Scheduler
open Printf

type handler_t = Handler of (unit -> unit) | Stop 

(* An event is composed of an event handler and a time at which the event is
   scheduled to occur. *)
type event_t = {
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


  method virtual private next_event : event_t option 

  method virtual private sched_event_at : ev:event_t -> unit

  method stop_at ~t =  s#sched_handler_at  ~handler:Stop ~t
    
  method stop_in ~t =  s#stop_at  ~t:(Time (t +. Time.get_time()))

  method private sched_handler_at ~handler ~t = (
    match t with 
      | ASAP -> 
	  s#sched_event_at {handler=handler; time=Time.get_time()};
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
	  s#sched_event_at {handler=handler; time=t};

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

  method sched_at ~f ~t = s#sched_handler_at ~handler:(Handler f) ~t
    
  method sched_in ~f ~t = s#sched_at ~f ~t:(Time (t +. Time.get_time()))

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

end

class  schedList = 
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

class  schedHeap = 
object(s)
  inherit sched

  initializer 
    s#set_objdescr "/sched/heap"

  val heap = EventHeap.create 1024

  method private next_event =  
    if EventHeap.is_empty heap then 
      None 
    else 
      Some (EventHeap.pop_maximum  heap)
    
  method private sched_event_at ~ev = 
    EventHeap.add  heap ev

end

let (sched_:sched option ref) = ref None 
let s () = o2v !sched_
let set_sched s = sched_ := Some s

let () = 
  set_sched (new schedHeap)


