(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(** Globally accessible parameters.  *)

let nodes = 
  Param.intcreate 
    ~name:"nodes" 
    ~default: 100
    ~doc:"Number of nodes in Simulation"
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
    ~name:"x_pix_size" 
    ~doc:"X [pix] size  of simulation area"
    ()

let y_pix_size = 
  Param.intcreate 
    ~name:"y_pix_size" 
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

(* These two lists contain the params that should be cmdline specifyable *)
let intparams = [nodes; ntargets]
let floatparams = [x_size; y_size; rrange]



let make_argspeclist () = 
  List.map (fun p -> Param.make_intargspec p) intparams
  @
  List.map (fun p -> Param.make_floatargspec p) floatparams
