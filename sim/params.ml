(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* changelog: 
*)


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

let x_size = 
  Param.create 
    ~name:"x_size" 
    ~default: None
    ~doc:"X [m] size  of simulation area"
    ~reader:float_of_string 
    ~checker:(Some 
      (fun s ->
	if s <= 0.0 then 
	  raise (Param.IllegalValue "x_size must be a positive float");
      )
    )

let y_size = 
  Param.create 
    ~name:"y_size" 
    ~default: None
    ~doc:"Y [m] size of simulation area"
    ~reader:float_of_string 
    ~checker:(Some 
      (fun s ->
	if s <= 0.0 then 
	  raise (Param.IllegalValue "y_size must be a positive float");
      )
    )

let rrange = 
  Param.create 
    ~name:"rrange" 
    ~default: None
    ~doc:"Radio range [m] of nodes"
    ~reader:float_of_string 
    ~checker:(Some 
      (fun s ->
	if s <= 0.0 then 
	  raise (Param.IllegalValue "rrange must be a positive float");
      )
    )

let ntargets = 
  Param.create 
    ~name:"ntargets" 
    ~default: (Some 1)
    ~doc:"Number of targets in Simulation"
    ~reader:int_of_string 
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

