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

let mobs = Hashtbl.create 64 

class discreteRandomWalk 
  (owner:#Node.node) 
  ?gran 
  () = 
object(s)

  inherit [unit] Mob_base.mobility owner ?gran ()
  inherit Mob_base.no_other_state_mixin

  initializer 
    Hashtbl.add mobs owner#id (s :> discreteRandomWalk) 



  (* ignores gran, meaningless for a discrete mob *)
  method getnewpos ~gran = 
    let pos = ((World.w())#nodepos owner#id) in
    let step = 
      [|
	(gran, 0.); 
	(-. gran, 0.); 
	(0., gran); 
	(0., -. gran)
      |].(Random.int rnd 4) in
    let newx = ref (xx (pos +++. step)) 
    and newy = ref (yy (pos +++. step)) in
(*    Printf.printf "Mob.ml: newx %f newy %f\n" !newx !newy; flush stdout;      *)

    if (!newx < 0.0) then newx := 0.;
    if (!newy < 0.0) then newy := 0.;
    if (!newx > (Param.get Params.x_size)) then newx := ((Param.get
      Params.x_size) -. 1.);
    if (!newy > (Param.get Params.y_size)) then newy := ((Param.get
      Params.y_size) -. 1.);
    (World.w())#boundarize (!newx, !newy)

end

let make_discrete_rw ?gran n = ignore (new discreteRandomWalk n ?gran ())

module Persist : Persist.t = 
struct 

  type state = Mob_base.base_state * unit

  let save oc = 
    let len = Misc.hashlen mobs in
    if len > 0 then
      Log.log#log_notice (lazy (sp "Saving %d random walks..." len));
    Marshal.to_channel oc len [];
    Hashtbl.iter 
      (fun nid mob -> Marshal.to_channel oc (nid, mob#dump_state()) []) mobs

  let restore ic = 
    let nmobs = (Marshal.from_channel ic : int) in
    if nmobs > 0 then
      Log.log#log_notice (lazy (sp "Restoring random walks..."));
    for i = 0 to nmobs - 1 do
      let (nid, mob_state) = 
	(Marshal.from_channel ic : Common.nodeid_t * state) in
      let mob = new discreteRandomWalk (Nodes.node nid) () in
      mob#restore_state mob_state;
      Log.log#log_info (lazy (sp "Restored random walk for node %d" nid))
    done;
    if nmobs > 0 then
      Log.log#log_notice (lazy (sp "Done. (restoring Random Walks)"));
    
end
