(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)





let mob_of_string = function
  | "borderwp" | "borderwaypoint" | "border" -> Mobs.Borderwaypoint
  | "waypoint" | "uniwaypoint" | "uniwp" -> Mobs.Uniwaypoint
  | "epfl" -> Mobs.Epfl_waypoint
  | "rw" | "randomwalk" -> Mobs.Randomwalk
  | "billiard" -> Mobs.Billiard
  | "none" -> Mobs.None
  | _ -> raise (Failure "Invalid format for mobility type")

let string_of_mob = function
  | Mobs.Borderwaypoint -> "borderwp" 
  | Mobs.Uniwaypoint -> "uniwaypoint"
  | Mobs.Epfl_waypoint -> "epfl"
  | Mobs.Randomwalk -> "rw"
  | Mobs.Billiard -> "billiard"
  | Mobs.None -> "none"

let mob = 
  Param.create ~name:"mob" ~default:Mobs.Billiard ~cmdline:true
    ~doc:"Mobility process" ~reader:mob_of_string ~printer:string_of_mob ()

let mob_array = ref ([||]: Mob.t array)

let set_speed_mps ?nidopt speed = 
  match nidopt with 
    | None ->
	Array.iter (fun m -> m#set_speed_mps speed) !mob_array
    | Some nid ->
	(!mob_array.(nid))#set_speed_mps speed

let get_speed_mps nid = (!mob_array.(nid))#speed_mps

let make_uniwaypoint_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mobs.uniwaypoint n ?gran ()))

let make_borderwaypoint_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mobs.borderwaypoint n ?gran ()))

let make_billiard_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mobs.billiard n ?gran ()))

let make_discrete_randomwalk_mobs ?gran () = mob_array := 
  (Nodes.map (fun n -> new Mobs.discreteRandomWalk n ?gran ()))

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

