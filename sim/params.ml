(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* Globally accessible parameters.                                       *)
(* These parameters have not (yet?) been examined to figure out if they  *)
(* really need to be globally acessible.                                 *)

let warmup_percent = 
  Param.create 
    ~name:"warmup" 
    ~default: (Some 0.4)
    ~doc:"encounter ratio required to exit warmup" 
    ~reader:float_of_string
    ~checker:(Some 
      (fun s ->
	if s < 0.0 || s > 1.0 then 
	  raise (Param.IllegalValue "warmup must be a float between 0.0 and 1.0");
      )
    )
    
let minrange = 
  Param.create 
    ~name:"minrange" 
    ~default: (Some 0.0)
    ~doc:"minumum range of routes to compute" 
    ~reader:float_of_string
    ~checker:(Some
      (fun  s ->
	if s < 0.0  then 
	  raise (Param.IllegalValue "minrange must be a float between greater than 0.0");
      )
    )
    
let maxrange = 
  Param.create 
    ~name:"maxrange" 
    ~default: None
    ~doc:"maximum range of routes to compute" 
    ~reader:float_of_string
    ~checker:(Some
      (fun  s ->
	if s < 0.0  then 
	  raise (Param.IllegalValue "maxrange must be a float between greater than 0.0");
      )
    )

let warmup = 
  Param.create 
    ~name:"warmup" 
    ~default: (Some false)
    ~doc:"Warmup (move nodes around)"
    ~reader:bool_of_string 
    ~checker:None

let nodes = 
  Param.create 
    ~name:"nodes" 
    ~default: (Some 100)
    ~doc:"Number of nodes in Simulation"
    ~reader:int_of_string 
    ~checker:None

let ntargets = 
  Param.create 
    ~name:"ntargets" 
    ~default: (Some 1)
    ~doc:"Number of targets in Simulation"
    ~reader:int_of_string 
    ~checker:None

let algo = 
  Param.create 
    ~name:"algo" 
    ~default: (Some Common.GRADIENT)
    ~doc:"Routing algorithm to use"
    ~reader:Common.algo_of_string 
    ~checker:None

let mob = 
  Param.create 
    ~name:"mob" 
    ~default: (Some Common.RANDOMWALK)
    ~doc:"Mobility pattern to use in warmup"
    ~reader:Common.mobility_of_string 
    ~checker:None

(*
let top = 
  Param.create 
    ~name:"top" 
    ~default: (Some Common.DISCRETE)
    ~doc:"Topology to use"
    ~reader:Common.topology_of_string 
    ~checker:None
  *)

let action = 
  Param.create 
    ~name:"action" 
    ~default: (Some Common.SHOW_ROUTES)
    ~doc:"What to do!!"
    ~reader:Common.action_of_string 
    ~checker:None

let nodes_file = 
  Param.create 
    ~name:"nodes_file" 
    ~default: (Some "ler-sim.mld")
    ~doc:"File containing warmed up node positions"
    ~reader:Misc.id 
    ~checker:None

let trace_enabled = 
  Param.create 
    ~name:"trace_enabled" 
    ~default: (Some false)
    ~doc:"Save naml trace"
    ~reader:bool_of_string 
    ~checker:None

