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

open Complex
open Coord
open Misc
module Random = Random.State 

let sp = Printf.sprintf

type other_state = {
  direction : Complex.t;
  time_next_dir_change : Time.t
}

let mobs = Hashtbl.create 64 

class billiard
  (owner:#Node.node) 
  ?gran
  () = 
object(s)
  inherit [other_state] Mob_base.mobility owner ?gran  ()

  val mutable dir = {re=0.0; im=0.0}
    (* normalized complex representing direction. *)

  val mutable time_next_dir_change = Time.get_time()

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/billiard";
    Hashtbl.add mobs owner#id (s :> billiard) ;
    dir <- s#new_direction
  )

  method private new_direction = 
      let rad = (Random.float rnd (2. *. Misc.pi)) -. Misc.pi in 
      polar 1.0 rad;

  method private change_dir_maybe = 
    if Time.get_time() > time_next_dir_change then  (

      dir <- s#new_direction;

      let lambda = 1. /. ((Param.get Params.x_size) /. speed_mps ) in 
      (* The inter-change time is an expo; mean is the time to traverse
	 distance of network size. *)
      let delta_next_change = Misc.expo ~rand:(Random.float rnd 1.0) ~lambda in
      time_next_dir_change <- (Time.get_time()) +. delta_next_change
    )



  method getnewpos ~gran = (
    s#change_dir_maybe;

    
    let oldpos = (World.w())#nodepos owner#id in
    if (((World.w())#boundarize oldpos) <> oldpos) then (
      Printf.printf "%s\n %s\n" (Coord.sprintf ((World.w())#boundarize
	oldpos)) (Coord.sprintf oldpos);
      flush stdout;
    );

    assert (((World.w())#boundarize oldpos) = oldpos);
    
    let newx, newy = (oldpos +++. ((dir.re, dir.im)) ***. gran) in
    
    let hit_east_border = newx <= 0.0
    and hit_west_border = newx >= (Param.get Params.x_size)
    and hit_south_border = newy <= 0.0
    and hit_north_border = newy >= (Param.get Params.y_size) 
    in

    let newpos = 
    if (hit_east_border || hit_west_border) &&
      (hit_south_border || hit_north_border)
    then 
      begin              
	dir <- {re=(minus dir.re); im=(minus dir.im)};
	((newx, newy) +++. ((dir.re, dir.im)) ***. gran)
      end
    else if (hit_east_border || hit_west_border) then 
      begin              
	dir <- {dir with re=(minus dir.re)};
	((newx, newy) +++. ((dir.re, dir.im)) ***. gran)
      end
    else if (hit_north_border || hit_south_border) then  
      begin
	dir <- {dir with im=(minus dir.im)};
	((newx, newy) +++. ((dir.re, dir.im)) ***. gran)
      end
    else 
      newx, newy
    in
      ((World.w())#boundarize newpos)
  )

  method private dump_other_state() = 
    {direction=dir; 
    time_next_dir_change=time_next_dir_change}

  method private restore_other_state state = 
    dir <- state.direction;
    time_next_dir_change <- time_next_dir_change;
    if time_next_dir_change < Time.time() then
      s#log_warning (lazy 
	"Billiard#restore_other_state: time to next dir change is in the past!")

end

let make_billiard ?gran n = ignore (new billiard n ?gran ())

module Persist : Persist.t = 
struct 
  type state = Mob_base.base_state * other_state

  let save oc = 
    let len = Misc.hashlen mobs in
    if len > 0 then
      Log.log#log_notice (lazy (sp "Saving %d billiard mob. processes..." len));
    Marshal.to_channel oc len [];
    Hashtbl.iter 
      (fun nid mob -> Marshal.to_channel oc (nid, ((mob#dump_state()) : state)) []) mobs

  let restore ?(verbose=false) ic = 
    let nmobs = (Marshal.from_channel ic : int) in
    if nmobs > 0 then
      Log.log#log_notice (lazy (sp "Restoring %d billiard mob. processes..." nmobs));
    for i = 0 to nmobs - 1 do
      let (nid, mob_state) = 
	(Marshal.from_channel ic : Common.nodeid_t * state) in
      let mob = new billiard (Nodes.node nid) () in
      mob#restore_state mob_state;
      Log.log#log_info (lazy (sp "Restored billiard mob. processes for node %d" nid))
    done;
    if nmobs > 0 then
      Log.log#log_notice (lazy (sp "Done. (restoring billiard mob. processes)"));
    
end

