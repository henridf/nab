(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)







let mob_array = ref ([||]: Mob.t array)

let set_speed_mps ?nidopt speed = 
  match nidopt with 
    | None ->
	Array.iter (fun m -> m#set_speed_mps speed) !mob_array
    | Some nid ->
	(!mob_array.(nid))#set_speed_mps speed

      

let make_uniwaypoint_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mobs.uniwaypoint n ?gran ()))

let make_borderwaypoint_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mobs.borderwaypoint n ?gran ()))

let make_billiard_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mobs.billiard n ?gran ()))

let make_discrete_randomwalk_mobs() = mob_array := 
  (Nodes.gpsmap (fun n -> new Mobs.discreteRandomWalk n ()))

let make_epfl_waypoint_mobs() = (
  mob_array := (Nodes.map (fun n -> new Mobs.epfl_waypoint n ()));
  Array.iter 
    (fun m -> m#set_speed_mps 1.0
    ) !mob_array
)



let start_node i = !mob_array.(i)#start
let stop_node i = !mob_array.(i)#stop
let start_all() = Array.iteri (fun i _ -> start_node i) !mob_array
let stop_all() = Array.iteri (fun i _ -> stop_node i) !mob_array
