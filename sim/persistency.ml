(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Mods

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
  3. state : The raw data. For now this is only nodedbs and node
     positions. Complete node state (what agents it has instanciated, their
     state, etc) might be non trivial, so for now we only store nodeDBs, since
     that is what takes longest to generate and is reusable.
*)


type state_hdr_t = {
  node_cnt:int;
  time:Common.time_t
}

type sim_state_t = Simplenode.node_state_t array
  
let save_node_state ?(gpsnodes=false) oc = (
  let node_cnt = (Param.get Params.nodes) in
  let descr = sprintf "mws datafile\n Parameters:\n nodes: %d\n time: %f\n" 
    node_cnt 
    (Common.get_time())
  in
  let state_hdr = {
    node_cnt=node_cnt;
    time=(Common.get_time())
  }
  in

  let (node_states:sim_state_t) = 
    if gpsnodes then
     Nodes.gpsmap (fun n -> n#dump_state)
    else
      Nodes.map (fun n -> n#dump_state)
  in 

  Marshal.to_channel oc descr [];
  Marshal.to_channel oc state_hdr [];
  Marshal.to_channel oc node_states [];

  Log.log#log_notice (lazy "Saving node state..");

  close_out oc
)


let read_node_state ?(gpsnodes=false) ic  = 
(
  (* get bits from channel *)
  let str = (Marshal.from_channel ic : string) in
  let hdr = (Marshal.from_channel ic : state_hdr_t) in
  Log.log#log_notice (lazy 
      (sprintf "Restoring node state..."));

  let (node_states:sim_state_t) = 
    (Marshal.from_channel ic : sim_state_t) in
  
  if (hdr.node_cnt <> Array.length node_states) then 
    raise (Failure 
      "Persistency.read_state: node_cnt was different than length of Nodes array");
  
  if (hdr.node_cnt <> (Param.get Params.nodes)) then 
    raise (Failure 
      "Persistency.read_state: node_cnt was different than Params.nodes");
  
  (* set globals *)
  Common.set_time hdr.time;

  Log.log#log_notice (lazy "Simulation state read in:");
  Log.log#log_notice (lazy (sprintf "\t Nodes: %d" hdr.node_cnt));
  Log.log#log_notice (lazy (sprintf "\t Time: %f" hdr.time));

  if gpsnodes then (
  (* make node objects out of restored node states *)
    Nodes.set_gpsnodes
      (Array.mapi
	(fun i nodestate -> 
	  (new Gpsnode.gpsnode ~pos_init:nodestate i)
	) node_states);
    
    (* set up initial node position in internal structures of World.object *)
    Nodes.gpsiter (fun n -> (World.w())#init_pos ~nid:n#id ~pos:n#pos );
  ) 
  else (
    (* No state to restore in the node objects themselves. 
    Script_utils.make_nodes ~with_positions:false ();*)

    (* set up initial node position in internal structures of World.object *)
    Nodes.iteri (fun nid _ -> 
      (World.w())#movenode ~nid ~newpos:node_states.(nid));

  );
  assert ((World.w())#neighbors_consistent);
)

let save_grep_agents oc = 
  let states = 
    Array.map !Grep_agent.agents_array 
    (fun agent -> agent#get_state ())
  in Marshal.to_channel oc states [];
  Log.log#log_notice (lazy "Saving grep_agents state..")


let read_grep_agents ?(stack=0) ic = 
  let state_arr = (Marshal.from_channel ic : Grep_agent.grep_state_t array) in
  Log.log#log_notice (lazy 
      (sprintf "Restoring grep_agents..."));
  if Array.length state_arr  <> (Param.get Params.nodes) then (
    Log.log#log_error (lazy 
      (sprintf "Read in array of %d grep_agents, but there are %d nodes!!!"
	(Array.length state_arr)
	(Param.get Params.nodes)));
    exit (-1);
  );
  
  Grep_agent.set_agents
    (Nodes.map (fun n -> new Grep_agent.grep_agent n));
  Array.iteri !Grep_agent.agents_array
    (fun i n -> n#set_state state_arr.(i));
  
  Nodes.iter (fun n -> n#remove_rt_agent ~stack ());

  Nodes.iteri (fun i n -> 
    n#install_rt_agent ~stack (!Grep_agent.agents_array.(i) :> Rt_agent.t));





