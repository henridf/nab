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

open Coord

module Random = Random.State 

let sp = Printf.sprintf

type wp_type = Uni | Border
type other_state = 
    {target : Coord.coordf_t; wp_type : wp_type}

let mobs = Hashtbl.create 64 

class waypoint
  ?gran
  ?state:other_state
  (owner:#Node.node) 
  wp_type =
object(s)

  inherit [other_state] Mob_base.mobility owner ?gran  ()

  val mutable target_ = (0.0, 0.0)
  val mutable wp_type = wp_type

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/waypoint";
    Hashtbl.add mobs owner#id (s :> waypoint) ;
    target_ <- s#new_waypoint()
  )
  method virtual private new_waypoint : unit -> Coord.coordf_t
    
  method getnewpos ~gran = (
    
    let pos = (World.w())#nodepos owner#id in
    assert (((World.w())#boundarize pos) = pos);
    if ((World.w())#dist_coords target_ pos) <= gran then (
      (* arrived within gran[m] of target *)
      let oldtarget = target_ 
      in
      target_ <- s#new_waypoint();
      oldtarget
    ) else (
      let direction =  (Coord.normalize (target_ ---. pos))   in
      (pos +++. (direction ***. gran))
    )
  )

  method private dump_other_state() = {target=target_; wp_type=wp_type}
  method private restore_other_state state = 
    target_ <- state.target;
    wp_type <- state.wp_type

  method private new_waypoint() = 
    match wp_type with
      | Uni -> (World.w())#random_pos
      | Border ->    
	  let x = Random.float rnd (Param.get Params.x_size)
	  and y = Random.float rnd (Param.get Params.y_size) 
	  in
	  match (Random.bool rnd , Random.bool rnd) with
	    | true, true -> x, (Param.get Params.y_size)
	    | true, false -> x, 0.0
	    | false, true -> 0.0, y
	    | false, false -> (Param.get Params.y_size), y
end


let make_uniwaypoint ?gran n = ignore (new waypoint ?gran n Uni )
let make_borderwaypoint ?gran n = ignore (new waypoint ?gran n Border)

module Persist : Persist.t = 
struct 
  type state = Mob_base.base_state * other_state

  let save oc = 
    let len = Misc.hashlen mobs in
    if len > 0 then
      Log.log#log_notice (lazy (sp "Saving %d waypoint mob. processes..." len));
    Marshal.to_channel oc len [];
    Hashtbl.iter 
      (fun nid mob -> Marshal.to_channel oc (nid, mob#dump_state()) []) mobs

  let restore ic = 
    let nmobs = (Marshal.from_channel ic : int) in
    if nmobs > 0 then
      Log.log#log_notice (lazy (sp "Restoring waypoint mob. processes..."));
    for i = 0 to nmobs - 1 do
      let (nid, mob_state) = 
	(Marshal.from_channel ic : Common.nodeid_t * state) in
      let mob = new waypoint (Nodes.node nid) (snd mob_state).wp_type in
      mob#restore_state mob_state;
      Log.log#log_info (lazy (sp "Restored waypoint mob. processes for node %d" nid))
    done;
    if nmobs > 0 then
      Log.log#log_notice (lazy (sp "Done. (restoring waypoint mob. processes)"));
    
end

