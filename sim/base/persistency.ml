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



(*
  the global simulator state is written to file as:
  
  descr: string  
  hdr: state_hdr_t (size of simulation, etc)
  state: sim_state_t
  
  1. descr is not used by mws per se. The intention is to keep a readable
     trace in the .mld, so that the user can do a 'strings xxx.mld' to figure
     out how many nodes, what type of scenario, etc, if e.g. the filename is 
     not self-descriptive enough.
  2. hdr is the structured way of representing the params that are
     described in descr. This is used by mws, for example to know how many
     nodes there are, and hence what the size of the array is.
  3. state : The raw data. For now this is only le_tab's and node
     positions. Complete node state (what agents it has instanciated, their
     state, etc) might be non trivial, so for now we only store le_tab, since
     that is what takes longest to generate and is reusable.
*)


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

  let restore ?(verbose=false) ic = 
  Log.log#log_notice (lazy (sp "Restoring node state..."));
   let n_nodes = (Marshal.from_channel ic : int) in
    if n_nodes <> Param.get Params.nodes then (
      raise (Failure (sp
	"Persist_Nodes.restore: Params.nodes is %d, yet there are %d nodes"
	(Param.get Params.nodes) n_nodes));
      exit (-1));

    Script_utils.make_nodes ~with_positions:false ();
    for nid = 0 to n_nodes - 1 do
      let node_state = (Marshal.from_channel ic : Node.node_state_t) in
      (World.w())#init_pos ~nid ~pos:(Node.state_pos node_state)
    done;
  assert ((World.w())#neighbors_consistent);
  Log.log#log_notice (lazy "Done.")

end

module Persist_World : Persist.t = struct

  let restore ?(verbose=false) ic = 
    Script_utils.init_world()
  let save oc = ()
end

let save_item oc item = 
  Marshal.to_channel oc item [];
  begin match item with
    | `Params -> Param.Persist.save oc
    | `Time -> Time.Persist.save oc
    | `Nodes -> Persist_Nodes.save oc
    | `World -> Persist_World.save oc
    | `Str_agents -> Str_agent.Persist.save oc
  end


let restore_item ~verbose ic = 
  let item = 
    (Marshal.from_channel ic : [> `Params | `Time | `Nodes | `Str_agents]) in
  begin match item with
    | `Params -> 
	Param.Persist.restore ~verbose ic;
	(* cmdline args overwrite config state*)
	Arg.current := 0;
	Script_utils.parse_args ();
    | `Time -> Time.Persist.restore ic;
	Log.log#log_notice (lazy (sp "Time restored to: %f" (Time.time())));
    | `Nodes -> Persist_Nodes.restore ~verbose ic
    | `World -> Persist_World.restore ~verbose ic
    | `Str_agents -> Str_agent.Persist.restore ~verbose ic
  end

let save_sim_items = [`Params; `Time; `World; `Nodes; `Str_agents]

let save_sim oc = 
  Marshal.to_channel oc (List.length save_sim_items) [];
  List.iter (fun item -> save_item oc item) save_sim_items
  
let restore_sim ?(verbose=true) ic = 
  let n_items = (Marshal.from_channel ic : int) in
  for i = 0 to n_items - 1 do 
    restore_item ~verbose ic
  done
    

