(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Script_utils


let setup () = (

  (* Set global parameters. For these simulations, we want to have a unit node
     density, over a square lattice, so the lattice size should be the square
     root of the # of nodes *)
  Param.set Params.nodes 10000; 
  Param.set Params.x_size 100.0; 
  Param.set Params.y_size 100.0;

  (* Radio range (this should not change) *)
  Param.set Params.rrange ( 1. +. epsilon_float);

  (* Set the number of targets in each nodes last-encounter table.
     For small simulations this can be equal to the # of nodes. 
     For large simulations, we usually reduce this, so that the total memory
     space for LE tables is #nodes * ntargets
  *)
  Param.set Params.ntargets (Param.get Param.nodes);



  (* Set global objects (scheduler and world). This must be done *after* the
     params above have been set. *)
  init_sched();
  init_world();

  (* Create gpsnodes. Gps nodes are geographically-aware, ie they know their
     position (unlike simplenodes which do not). *)
  make_gpsnodes();

)


(* this funny construct is equivalent to a 'main', ie this is where the action
   starts *)
let _ = 
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
      
  (* Attach a discrete randomwalk mobility process to each node *)
  Mob_ctl.make_discrete_randomwalk_mobs();

  

  (* Compute the average # of neighbors per node. Given unit node density,
     this should be close to 5 (4 neighboring nodes + self) 
  *)
  let avg = avg_neighbors_per_node() in
  Printf.printf "Average # ngbrs : %f\n" avg;


    
    
    
