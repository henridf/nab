(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(** Globally accessible parameters.  *)

let nodes = 
  Param.intcreate 
    ~name:"nodes" 
    ~default:1000
    ~cmdline:true
    ~doc:"Number of nodes"
    ()

let x_size = 
  Param.floatcreate 
    ~name:"x_size" 
    ~doc:"X [m] size  of simulation area"
    ()

let y_size = 
  Param.floatcreate 
    ~name:"y_size" 
    ~doc:"Y [m] size of simulation area"
    ()

let x_pix_size = 
  Param.intcreate 
    ~default:400
    ~name:"x_pix_size" 
    ~doc:"X [pix] size  of simulation area"
    ()

let y_pix_size = 
  Param.intcreate 
    ~name:"y_pix_size" 
    ~default:300
    ~doc:"Y [pix] size of simulation area"
    ()

let rrange = 
  Param.floatcreate 
    ~name:"rrange" 
    ~doc:"Radio range [m] of nodes"
    ()

let ntargets = 
  Param.intcreate 
    ~name:"ntargets" 
    ~default:1
    ~doc:"Number of targets in Simulation"
    ()

let mac = 
  Param.stringcreate ~name:"mac" ~default:"cont" ~cmdline:true
    ~doc:"Mac layer" ~checker:(fun s -> (Mac.strset_mac s)) ()

let log_level = 
  Param.stringcreate ~name:"loglevel" ~default:"warn" ~cmdline:true
    ~doc:"Log level"  ~checker:(fun s -> Log.strset_log_level s) ()

