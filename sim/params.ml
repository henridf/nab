(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(** Globally accessible parameters.  *)

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

let x_pix_size = 
  Param.create 
    ~name:"x_pix_size" 
    ~default: None
    ~doc:"X [pix] size  of simulation area"
    ~reader:int_of_string 
    ~checker:(Some 
      (fun s ->
	if s <= 0 then 
	  raise (Param.IllegalValue "x_pix_size must be positive");
      )
    )

let y_pix_size = 
  Param.create 
    ~name:"y_pix_size" 
    ~default: None
    ~doc:"Y [pix] size of simulation area"
    ~reader:int_of_string 
    ~checker:(Some 
      (fun s ->
	if s <= 0 then 
	  raise (Param.IllegalValue "y_pix_size must be positive");
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


