open Misc

let (sched_:Sched.scheduler_t option ref) = ref None 
let sched () = o2v !sched_
let set_sched s = sched_ := Some s
