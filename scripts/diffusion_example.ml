





let setup () = (

  (* Set global parameters. For these simulations, we want to have a unit node
     density, over a square lattice, so the lattice size should be the square
     root of the # of nodes *)
  Param.set Params.nodes 25; 
  Param.set Params.x_size 5.0; 
  Param.set Params.y_size 5.0;

  (* Radio range (this should not change) *)
  Param.set Params.rrange ( 1. +. epsilon_float);

  (* Set the number of targets in each nodes last-encounter table.
     For small simulations this can be equal to the # of nodes. 
     For large simulations, we usually reduce this, so that the total memory
     space for LE tables is #nodes * ntargets
  *)
  Param.set Params.ntargets (Param.get Params.nodes);



  (* Set global objects (scheduler and World). This must be done *after* the
     params above have been set. *)
  Script_utils.init_sched();
  Script_utils.init_lazy_world();

  (* Create gpsnodes. Gps nodes are geographically-aware, ie they know their
     position (unlike simplenodes which do not). *)
  Script_utils.make_gpsnodes();

)


(* this funny construct is equivalent to a 'main', ie this is where the action
   starts *)
let _ = 

  Script_utils.parse_args();

  setup();
  
  (* At creation, nodes have random positions in the surface. We now
     discretize these positions so that the nodes are only in discrete lattice
     locations. *)
  Nodes.gpsiter (fun n -> 
    let p = n#pos in 
    let discr_p = (Coord.coord_floor p) in
    n#move discr_p
  );
  
  (* Attach a periodic_hello_agent to each node, and keep them all the agents 
     in an array called hello_agents *)
  let hello_agents = 
    Nodes.gpsmap 
      (fun n -> new Hello_agents.periodic_hello_agent n) in
  
  (* This function computes the encounter ratio, ie the proportion of node pairs for which
    encounter entries exist *)
  let proportion_met_nodes()   = 
    let targets = Param.get Params.ntargets in
    let total_encounters = 
      Array.fold_left (fun encs agent -> (agent#db#num_encounters) + encs) 0 hello_agents
    in
    (float total_encounters) /. (float ((Param.get Params.nodes) * targets))
  in

  (* Attach a discrete randomwalk mobility process to each node *)
  Mob_ctl.make_discrete_randomwalk_mobs();
  Mob_ctl.start_all();

  (* Compute the average # of neighbors per node. Given unit node density,
     this should be close to 5 (4 neighboring nodes + self) 
  *)
  let avg = Script_utils.avg_neighbors_per_node() in
  Printf.printf "Average # ngbrs : %f\n" avg;

  (* Compute the 'encounter ratio', ie the number of encounters in each node's
     database. At this point, the hello_agents have not done broadcast
     anything yet, so the all encounter tables should be empty, ie 0.0
     encounter ratio. *)
  let enc_ratio = proportion_met_nodes() in
  Printf.printf "Pre-hello encounter ratio %f \n" enc_ratio;
  
  (* Make the simulation run for 1.01 simulated seconds.
     This will give the periodic_hello_agents time to each broadcast one hello packet
     (since they each broadcast every second)
  *)
  (Sched.s())#run_for ~duration:10.01;

  (* Recompute the encounter ratio. It should be non-zero now that nodes have
     made one hello broadcast. *)
  let enc_ratio = proportion_met_nodes() in
  Printf.printf "Post-hello encounter ratio %f \n" enc_ratio;
