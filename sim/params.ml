(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Globally accessible parameters.                                       *)
(* These parameters have not (yet?) been examined to figure out if they  *)
(* really need to be globally acessible.                                 *)

let gui = 
  Param.create 
    ~name:"gui" 
    ~default:true 
    ~doc:"launch UI"
    ~reader:bool_of_string 
    ~checker:None

let warmup_percent = 
  Param.create 
    ~name:"warmup" 
    ~default:0.4 
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
    ~default:0.0 
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
    ~default:0.0 
    ~doc:"maximum range of routes to compute" 
    ~reader:float_of_string
    ~checker:(Some
      (fun  s ->
	if s < 0.0  then 
	  raise (Param.IllegalValue "maxrange must be a float between greater than 0.0");
      )
    )

let dowarmup = 
  Param.create 
    ~name:"warmup" 
    ~default:true 
    ~doc:"Warmup (move nodes around)"
    ~reader:bool_of_string 
    ~checker:None

let dorouting = 
  Param.create 
    ~name:"warmuponly" 
    ~default:true 
    ~doc:"Exit after warmup"
    ~reader:bool_of_string 
    ~checker:None

let nodes = 
  Param.create 
    ~name:"nodes" 
    ~default:100 
    ~doc:"Number of nodes in Simulation"
    ~reader:int_of_string 
    ~checker:None

let ntargets = 
  Param.create 
    ~name:"ntargets" 
    ~default:1 
    ~doc:"Number of targets in Simulation"
    ~reader:int_of_string 
    ~checker:None

let algo = 
  Param.create 
    ~name:"algo" 
    ~default:Common.GRADIENT 
    ~doc:"Routing algorithm to use"
    ~reader:Common.algo_of_string 
    ~checker:None

let mob = 
  Param.create 
    ~name:"mob" 
    ~default:Common.RANDOMWALK 
    ~doc:"Mobility pattern to use in warmup"
    ~reader:Common.mobility_of_string 
    ~checker:None

let top = 
  Param.create 
    ~name:"top" 
    ~default:Common.DISCRETE 
    ~doc:"Topology to use"
    ~reader:Common.topology_of_string 
    ~checker:None

let nodes_file = 
  Param.create 
    ~name:"nodes_file" 
    ~default:"ler-sim.mld" 
    ~doc:"File containing warmed up node positions"
    ~reader:Misc.id 
    ~checker:None

