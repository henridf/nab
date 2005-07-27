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



let sp = Printf.sprintf

module Persist_Nodes : Persist.t = struct
  (* could not go in node.ml because of reference to script_utils*)

  let save oc = 
    Log.log#log_notice (lazy "Saving node state..");
    let n_nodes = 
      Nodes.fold (fun _ tot -> tot + 1) 0
    in
    if n_nodes <> Param.get Params.nodes then
      raise (Failure (sp "Params.nodes is %d, yet there are %d nodes!!?!"
	(Param.get Params.nodes) n_nodes));
    Marshal.to_channel oc n_nodes [];
    Nodes.iter (fun n -> Marshal.to_channel oc n#dump_state []);
    Log.log#log_notice (lazy "Done saving node state.")


  let restore ic = 

    Log.log#log_notice (lazy (sp "Restoring node state..."));
    let n_nodes = (Marshal.from_channel ic : int) in
    if n_nodes <> Param.get Params.nodes then (
      raise (Failure (sp
	"Persist_Nodes.restore: Params.nodes is %d, yet there are %d nodes"
	(Param.get Params.nodes) n_nodes));
      exit (-1));
    
    Script_utils.make_nodes ();
    for nid = 0 to n_nodes - 1 do
      let node_state = (Marshal.from_channel ic : Node.node_state_t) in
      (World.w())#movenode ~nid ~newpos:(Node.state_pos node_state)
    done;
    assert ((World.w())#neighbors_consistent);
    Log.log#log_notice (lazy "Done (restoring node state).")
      
end

module Persist_World : Persist.t = struct
  
  let restore ic = 
    Script_utils.init_world()
  let save oc = ()
end





type persistable_item = 
    [ `Params 
    | `Time 
    | `World 
    | `Nodes 
    | `Mobs
    | `Rt_agents]

let save_item oc item = 
  Marshal.to_channel oc item [];
  begin match item with
    | `Params -> Param.Persist.save oc
    | `Time -> Time.Persist.save oc
    | `Nodes -> Persist_Nodes.save oc
    | `Mobs -> Mob_ctl.Persist.save oc
    | `World -> Persist_World.save oc
    | `Rt_agents -> Rt_agent_persist.Persist.save oc
  end
  
let restore_item ic = 
  let item = 
    (Marshal.from_channel ic : persistable_item) in
  begin match item with
    | `Params -> 
	Param.Persist.restore ic;
	(* cmdline args overwrite config state*)
	Arg.current := 0;
	Script_utils.parse_args ();
    | `Time -> Time.Persist.restore ic;
	Log.log#log_notice (lazy (sp "Time restored to: %f" (Time.time())));
    | `Nodes -> Persist_Nodes.restore ic
    | `Mobs -> Mob_ctl.Persist.restore ic
    | `World -> Persist_World.restore ic
    | `Rt_agents -> Rt_agent_persist.Persist.restore ic
  end

let save_sim_items = 
  ([`Params; `Time; `World; `Nodes; `Mobs; `Rt_agents] : 
  [> persistable_item ] list)
  (* this cast is to 'open' the type, since when it is read back the type may
     have grown. not sure if not doing this would be dangerous, but playing it
     safe anyway...*)
  

let save_sim oc = 
  Marshal.to_channel oc (List.length save_sim_items) [];
  List.iter (fun item -> save_item oc item) save_sim_items
  
let restore_sim ?(verbose=true) ic = 
  let n_items = (Marshal.from_channel ic : int) in
  for i = 0 to n_items - 1 do 
    restore_item ic
  done
    

let get_config ic = 
  let n_items = (Marshal.from_channel ic : int) in
  let item = 
    (Marshal.from_channel ic : persistable_item) in
  begin
    match item with
      | `Params -> 
	  Param.Persist.restore ic;
	  (* cmdline args overwrite config state*)
      | _ -> failwith "Persistency.getconfig: Expected to find Params item first"
  end
  
