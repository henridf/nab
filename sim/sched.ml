(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf

type handler_t = (unit -> unit)
type sched_time_t = | ASAP (* schedule as soon as possible *)
		    | ALAP (* schedule as late as possible *)
		    | Time of Common.time_t  (* schedule at given time *)

type event_t = {
  handler:handler_t;
  time:Common.time_t;
}

type eventlist_entry_t = 
  | Event of event_t
  | Stop of event_t          (* Inserted by stop_ methods *)

class type scheduler_t = 
object 

  (* keep processing queued events until none left *)
  method run : unit -> unit 

  (* same as #run except that continure is called between each event, and
     processing stops if f returns true *)
  method run_until : 
    continue:(unit -> bool) -> 
    unit 

  method objdescr : string

  method sched_in : handler:handler_t -> t:Common.time_t -> unit
  method sched_at : handler:handler_t -> t:sched_time_t -> unit
  method sched_event_at : ev:eventlist_entry_t -> unit

  method stop_in :  t:Common.time_t -> unit
  method stop_at :  t:sched_time_t -> unit

  method private next_event : eventlist_entry_t option 
end

let event_time ev = 
  match ev with 
    | Event e -> e.time
    | Stop s -> s.time

let compare ev1 ev2 = 
  (event_time ev1) < (event_time ev2) 
  
class schedList : scheduler_t = 

object(s)

  inherit Log.loggable

  val ll = Linkedlist.create()

  initializer (
    objdescr <- "/sched/list"
  )
    
  method private next_event =  Linkedlist.pophead ~ll:ll 
    
  method sched_event_at ~ev = 
    Linkedlist.insert ~ll:ll ~v:ev ~compare:compare

  method stop_at ~t = (
    let str = ref "" in (

      match t with 
	| ASAP -> 
	    s#sched_event_at (Stop {handler=(fun () -> ()); time=Common.get_time()});
	| ALAP -> raise (Failure "schedList.sched: ALAP not implemented\n")
	| Time t -> (
	    assert (t > Common.get_time());
	    s#sched_event_at (Stop {handler=(fun () -> ()); time=t});
	  );
    );
  )
  method stop_in ~t =  s#stop_at  (Time (t +. Common.get_time()))

  method sched_at ~handler ~t = (
    let str = ref "" in (

      match t with 
	| ASAP -> 
	    s#sched_event_at (Event {handler=handler; time=Common.get_time()});
	| ALAP -> raise (Failure "schedList.sched: ALAP not implemented\n")
	| Time t -> (
	    assert (t > Common.get_time());
	    s#sched_event_at (Event {handler=handler; time=t});
	  );
    );
(*    s#log_debug (sprintf "scheduling event at %s" !str);*)
  )
    
  method sched_in ~handler ~t = s#sched_at handler (Time (t +. Common.get_time()))

  method run_until ~continue = (
    try 
      while (true) do 
	match s#next_event with
	  | None -> raise Misc.Break
	  | Some (Stop s) -> 
	      Common.set_time (s.time);
	      raise Misc.Break
	  | Some (Event ev) -> (
	      Common.set_time (ev.time);
	      ev.handler();
	      if (not (continue())) then 
		raise Misc.Break;
	    )
      done;
      
    with
      | Misc.Break -> () 
      | o -> raise o
    )

  method run() = s#run_until (fun () -> true)

end
