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


type mob_t = 
    [ `Borderwaypoint 
    | `Uniwaypoint 
    | `Epfl_waypoint
    | `Randomwalk 
    | `Billiard]

let sp = Printf.sprintf

let mob_of_string = function
  | "borderwp" | "borderwaypoint" | "border" -> `Borderwaypoint
  | "waypoint" | "uniwaypoint" | "uniwp" -> `Uniwaypoint
  | "epfl" -> `Epfl_waypoint
  | "rw" | "randomwalk" -> `Randomwalk
  | "billiard" -> `Billiard
  | "none" -> `None
  | _ -> raise (Failure "Invalid format for mobility type")

let string_of_mob = function
  | `Borderwaypoint -> "borderwp" 
  | `Uniwaypoint -> "uniwaypoint"
  | `Epfl_waypoint -> "epfl"
  | `Randomwalk -> "rw"
  | `Billiard -> "billiard"
  | `None -> "none"

let mob : [mob_t | `None] Param.t = 
  Param.create ~name:"mob" ~default:`Billiard ~cmdline:true
    ~doc:"Mobility process" ~reader:mob_of_string ~printer:string_of_mob ()
    

let set_speed nid speed = 
  (Mob_base.mob nid)#set_speed_mps speed

let set_speed_all speed = 
  Nodes.iteri (fun nid _ -> set_speed nid speed)

let get_speed nid = (Mob_base.mob nid)#speed_mps

let make_uniwaypoint_mobs ?gran () = 
  (Nodes.iter 
    (fun n -> Waypoint.make_uniwaypoint ?gran n))

let make_borderwaypoint_mobs ?gran () =
  (Nodes.iter 
    (fun n -> Waypoint.make_uniwaypoint ?gran n))

let make_billiard_mobs ?gran () = 
  (Nodes.iter 
    (fun n -> Billiard.make_billiard ?gran n))

let make_discrete_randomwalk_mobs ?gran () = 
  (Nodes.iter 
    (fun n -> Walk.make_discrete_rw ?gran n))

let make_epfl_waypoint_mobs() = (
  (Nodes.iter 
    (fun n -> Epfl_mob.make_epfl_waypoint n));
  set_speed_all 1.0;
)


let start_node i = (Mob_base.mob i)#start
let stop_node i = (Mob_base.mob i)#stop
let start_all() = Nodes.iteri (fun i _ -> start_node i) 
let stop_all() = Nodes.iteri (fun i _ -> stop_node i) 

module Persist : Persist.t = 
struct

type mob_t = 
    [ `Waypoint 
    | `Epfl_waypoint
    | `Randomwalk 
    | `Billiard]


let restore_mob ~verbose ic = 
  let mobtype = 
    (Marshal.from_channel ic : mob_t) in
  begin match mobtype with
    | `Waypoint -> Waypoint.Persist.restore ic
    | `Epfl_waypoint -> Epfl_mob.Persist.restore ic
    | `Randomwalk -> Walk.Persist.restore ic
    | `Billiard -> Billiard.Persist.restore ic
end

let save_mob oc mob = 
  Marshal.to_channel oc mob [];
  begin match mob with
    | `Waypoint -> Waypoint.Persist.save oc
    | `Epfl_waypoint -> Epfl_mob.Persist.save oc
    | `Randomwalk -> Walk.Persist.save oc
    | `Billiard -> Billiard.Persist.save oc
end

let mobs_to_save = 
  ([ `Waypoint; `Epfl_waypoint;
  `Randomwalk; `Billiard] : [> mob_t] list)
  (* this cast is to 'open' the type, since when it is read back the type may
     have grown. not sure if not doing this would be dangerous, but playing it
     safe anyway...*)


let save oc = 
  Marshal.to_channel oc (List.length mobs_to_save) [];
  List.iter (fun item -> save_mob oc item) mobs_to_save


let restore ?(verbose=false) ic = 
  Log.log#log_notice (lazy (sp "Deleting any existing mobility processes..."));

  Mob_base.delete_all_mobs();
  let n_mobs = (Marshal.from_channel ic : int) in
  if n_mobs > 0 then 
    Log.log#log_notice (lazy (sp "Restoring mobility processes..."));
  for i = 0 to n_mobs - 1 do 
    restore_mob ~verbose ic
  done;
  if n_mobs > 0 then 
    Log.log#log_notice (lazy (sp "Done. (restoring mobility processes)"))

end
