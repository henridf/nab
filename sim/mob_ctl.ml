(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

let mob_array = ref ([||]: #Mob.mobility array)

let set_speed_mps ?nidopt speed = 
  match nidopt with 
    | None ->
	Array.iter (fun m -> m#set_speed_mps speed) !mob_array
    | Some nid ->
	(!mob_array.(nid))#set_speed_mps speed

      

let make_uniwaypoint_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mob.uniwaypoint n ?gran ()))

let make_borderwaypoint_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mob.borderwaypoint n ?gran ()))

let make_discrete_randomwalk_mobs() = mob_array := 
  (Nodes.gpsmap (fun n -> new Mob.discreteRandomWalk n ()))

let make_epfl_waypoint_mobs() = (
  mob_array := (Nodes.gpsmap (fun n -> new Mob.epfl_waypoint n ()));
  Array.iter 
    (fun m -> m#set_speed_mps 1.0
    ) !mob_array
)



let start_node i = !mob_array.(i)#start
let stop_node i = !mob_array.(i)#stop
let start_all() = Array.iteri (fun i _ -> start_node i) !mob_array
let stop_all() = Array.iteri (fun i _ -> stop_node i) !mob_array
