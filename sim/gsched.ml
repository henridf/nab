open Misc

let (sched_:Sched.scheduler_t option ref) = ref None 
let sched () = o2v !sched_
let set_sched s = sched_ := Some s

(*
let a = ref 0 
let f n () = begin assert (!a = n); incr a end
let test = 
  set_sched (new Sched.schedList);
  Printf.printf "Beginning simple scheduler test\n";
  flush stdout;
  Common.set_time 0.0;
  (sched())#sched_in ~f:(f 0) ~t:1.0;
  (sched())#sched_in ~f:(f 1) ~t:1.1;
  (sched())#stop_at  ~t:(Sched.ASAP);
  (sched())#run();
  assert (Common.get_time() = 0.0);
  (sched())#run_for ~duration:1.0 ;
  assert (Common.get_time() = 1.0);
  (sched())#run();
  assert (Common.get_time() = 1.1);
  (sched())#sched_in 
  ~f:(fun () -> f 2 ();
    (sched())#sched_in ~f:(f 4) ~t:0.9;
    (sched())#stop_in ~t:0.85;
    
    (sched())#sched_in ~f:(f 3) ~t:0.8;
  )
    ~t:1.0;
  (sched())#run();
  assert (!a = 4);
  assert (Common.get_time() = 2.95);
  (sched())#run();

  Printf.printf "a is %d\n" !a;

  assert (!a = 5);
  assert (Common.get_time() = 3.0);






(* replace block below with
  (sched())#sched_at ~f:(f 6) ~t:(Sched.Time 5.0);

  (sched())#sched_at ~f:(f 5) ~t:(Sched.ASAP);


   for heap scheduler, which doesn't respect the 'FIFO' property for events
   scheduled at the same time *)

  (sched())#sched_at ~f:(f 8) ~t:(Sched.Time 5.0);
  (sched())#sched_at ~f:(f 9) ~t:(Sched.Time 5.0);
  (sched())#sched_at ~f:(f 10) ~t:(Sched.Time 5.0);

  (sched())#sched_at ~f:(f 5) ~t:(Sched.ASAP);
  (sched())#sched_at ~f:(f 6) ~t:(Sched.ASAP);
  (sched())#sched_at ~f:(f 7) ~t:(Sched.ASAP);


  Printf.printf "time is %f\n" (Common.get_time());
  (sched())#run();



  Printf.printf "End simple scheduler test\n";
  flush stdout;



  *)
