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
open Graph
open Misc

module Random = Random.State 

let sp = Printf.sprintf

let closest_epfl_node pos = (
  let d, ind = (ref max_float, ref (-1)) in 
  Graph.iteri_
    (fun i -> 
      let thisdist = 
	(World.w())#dist_coords (Read_coords.box_centeri i) pos 
      in
      if thisdist < !d then (
	d := thisdist;
	ind := i
      )
    ) (Read_coords.g());
  !ind
)

type other_state = 
    {graphtarget : int;
    graph_hops : Coord.coordf_t list;
    current_graph_pos : int}
  
let mobs = Hashtbl.create 64 
  
class epfl_waypoint 
  (owner:#Node.node) = 
object(s)
  inherit [other_state] Mob_base.mobility owner ()

  val mutable graphtarget_ = 0      (* as a graph node index *)
  val mutable graph_hops_ = []       (* remaining hops through the graph to
					graphtarget_ *)
  val mutable current_graph_pos_ = 0

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable) "/epfl_waypoint";
    Hashtbl.add mobs owner#id (s :> epfl_waypoint) ;
    current_graph_pos_ <- closest_epfl_node ((World.w())#nodepos owner#id);
    s#get_new_target;
  )
    
  method private get_new_target = (

    let g = graphtarget_ in
    (* to make sure we pick a different one *)
    while (g = graphtarget_) do
      graphtarget_ <-  Random.int rnd 113;
    done;

    current_graph_pos_ <- closest_epfl_node ((World.w())#nodepos owner#id);
    graph_hops_ <- 
    List.map (fun i -> Read_coords.box_centeri i) 
      ((Graph.routei_dij_ (Read_coords.g()) ~src:current_graph_pos_ ~dest:graphtarget_) @
      [graphtarget_]);

  )
    
  method getnewpos ~gran = (
    let next_hop_target = List.hd graph_hops_ in
    let pos = ((World.w())#nodepos owner#id) in
    if ((World.w())#dist_coords next_hop_target pos) <= gran then (
      begin
	match graph_hops_ with
	  | hd::[] -> s#get_new_target
	  | hd::rest -> graph_hops_ <- rest
	  | [] -> raise (Misc.Impossible_Case "Mob.epfl.getnewpos")
      end;
      next_hop_target
    ) else (
      let direction =  (Coord.normalize (next_hop_target ---. pos))   in
      (pos +++. (direction ***. gran))
    )
  )

  method private dump_other_state() = 
    {graphtarget = graphtarget_;
    graph_hops = graph_hops_;
    current_graph_pos = current_graph_pos_}
    
  method private restore_other_state state = 
    graphtarget_ <- state.graphtarget;
    graph_hops_ <- state.graph_hops;
    current_graph_pos_ <- state.current_graph_pos

   
end

let make_epfl_waypoint n = ignore (new epfl_waypoint n)



module Persist : Persist.t = 
struct 
  type state = Mob_base.base_state * other_state

  let save oc = 
    let len = Misc.hashlen mobs in
    if len > 0 then
      Log.log#log_notice (lazy (sp "Saving %d epfl mob. processes..." len));
    Marshal.to_channel oc len [];
    Hashtbl.iter 
      (fun nid mob -> Marshal.to_channel oc (nid, mob#dump_state()) []) mobs

  let restore ic = 
    let nmobs = (Marshal.from_channel ic : int) in
    if nmobs > 0 then
      Log.log#log_notice (lazy (sp "Restoring epfl mob. processes..."));
    for i = 0 to nmobs - 1 do
      let (nid, mob_state) = 
	(Marshal.from_channel ic : Common.nodeid_t * state) in
      let mob = new epfl_waypoint (Nodes.node nid) in
      mob#restore_state mob_state;
      Log.log#log_info (lazy (sp "Restored epfl mob. processes for node %d" nid))
    done;
    if nmobs > 0 then
      Log.log#log_notice (lazy (sp "Done. (restoring epfl mob. processes)"));
    
end

