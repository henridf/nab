(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

let mob_array = ref ([||]: #Mob.mobility array)

let make_waypoint_mobs() = mob_array := 
  (Nodes.map (fun n -> new Mob.waypoint n
    ((Gworld.world())#movenode ~nid:n#id)))

let make_epfl_waypoint_mobs() = (
  mob_array := (Nodes.gpsmap (fun n -> new Mob.epfl_waypoint n n#move));
  Array.iter 
    (fun m ->
      if (Random.int 2) = 0 then (
	m#set_speed_mps 1.0
      ) else (
	m#set_speed_mps 1.0
      )
    ) !mob_array
)

let start_node i = !mob_array.(i)#start
let stop_node i = !mob_array.(i)#stop
let start_all() = Array.iteri (fun i n -> start_node i) !mob_array
let stop_all() = Array.iteri (fun i n -> stop_node i) !mob_array
