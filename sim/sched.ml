(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf

type handler_t = Stop | Handler of (unit -> unit)
type sched_time_t = | ASAP (* schedule as soon as possible *)
		    | ALAP (* schedule as late as possible *)
		    | Time of Common.time_t  (* schedule at given time *)

type event_t = {
  handler:handler_t;
  time:Common.time_t;
}

class type virtual scheduler_t = 
object 

  (* keep processing queued events until none left *)
  method run : unit -> unit 

  (* same as #run except that continure is called between each event, and
     processing stops if continue() returns false *)
  method run_until : 
    continue:(unit -> bool) -> 
    unit 

  method run_for : duration:Common.time_t -> unit
    (* runs until no more events or duration secs passed, whichever
       occurs first*)

  method objdescr : string

  method sched_in : f:(unit -> unit) -> t:Common.time_t -> unit
  method sched_at : f:(unit -> unit) -> t:sched_time_t -> unit
  method virtual private sched_event_at : ev:event_t -> unit

  method stop_in :  t:Common.time_t -> unit
  method stop_at :  t:sched_time_t -> unit



  method virtual private next_event : event_t option 
end


let compare ev1 ev2 = ev1.time < ev2.time

  
class virtual sched  = 

object(s)

  inherit Log.loggable

  method virtual private next_event : event_t option 

  method virtual private sched_event_at : ev:event_t -> unit

  initializer (
    objdescr <- "/sched/list"
  ) 
    
    
  method stop_at ~t =  s#sched_handler_at  ~handler:Stop ~t
    
  method stop_in ~t =  s#stop_at  ~t:(Time (t +. Common.get_time()))

  method private sched_handler_at ~handler ~t = (
    let str = ref "" in (

      match t with 
	| ASAP -> 
	    s#sched_event_at {handler=handler; time=Common.get_time()};
	| ALAP -> raise (Failure "schedList.sched_at: ALAP not implemented\n")
	| Time t -> (
	    if (t <= Common.get_time()) then 
	      raise (Failure "schedList.sched_at: attempt to schedule an event in the past");
	    s#sched_event_at {handler=handler; time=t};
	  );
    );
(*    s#log_debug (sprintf "scheduling event at %s" !str);*)
  )

  method sched_at ~f ~t = s#sched_handler_at ~handler:(Handler f) ~t
    
  method sched_in ~f ~t = s#sched_at ~f ~t:(Time (t +. Common.get_time()))

  method run_until ~continue = (
    try 
      while (true) do 
	match s#next_event with
	  | None -> raise Misc.Break
	  | Some ev -> 
	      begin
		Common.set_time (ev.time);
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
    let t = (Common.get_time()) in
    let continue  = (fun () -> (Common.get_time()) < duration +. t) in
    s#run_until ~continue

  method run() = s#run_until ~continue:(fun () -> true)

end

class  schedList = 
object
  inherit sched

  val ll = Linkedlist.create()

  method private next_event =  Linkedlist.pophead ~ll:ll 
    
  method private sched_event_at ~ev = 
    Linkedlist.insert ~ll:ll ~v:ev ~compare:compare

end



module Compare =
struct
  type t = event_t
  let compare ev1 ev2 = 
    if  ev1.time > ev2.time  then -1 else 1
      
end
module EventHeap = Heap.Imperative (Compare)

class  schedHeap = 
object
  inherit sched

  val heap = EventHeap.create 1024 

  method private next_event =  
    if EventHeap.is_empty heap then 
      None 
    else 
      Some (EventHeap.pop_maximum  heap)
    
  method private sched_event_at ~ev = 
    EventHeap.add  heap ev

end
