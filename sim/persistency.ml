(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf

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
  ntargets:int;
  time:Common.time_t
}

type sim_state_t = Node.node_state_t array (* size Nodes_cnt * Nodes_cnt *)
  
let save_state ~node_cnt ~out_chan ~ntargets = (
  let descr = sprintf "mws datafile\n Parameters:\n nodes: %d\n time: %f\n" 
    node_cnt 
    (Common.get_time())
  in
  let state_hdr = {
    node_cnt=node_cnt;
    ntargets=ntargets;
    time=(Common.get_time())
  }
  in
  let (node_states:sim_state_t) = 
    Nodes.map (fun n -> n#dump_state)
  in

  if (node_cnt <> Array.length node_states) then 
    raise (Failure 
      "Persistency.save_state: node_cnt was different than length of Nodes array");
  
  Marshal.to_channel out_chan descr [];
  Marshal.to_channel out_chan state_hdr [];
  Marshal.to_channel out_chan node_states [];

  Log.log#log_info "Simulation state saved.";

  close_out out_chan
)


let read_state ~in_chan  = 
  raise Misc.Not_Implemented
(*
(
  (* get bits from channel *)
  let str = (Marshal.from_channel in_chan : string) in
  let hdr = (Marshal.from_channel in_chan : state_hdr_t) in
  let node_states = (Marshal.from_channel in_chan : sim_state_t) in
  
  if (hdr.node_cnt <> Array.length node_states) then 
    raise (Failure 
      "Persistency.read_state: node_cnt was different than length of Nodes array");
  
  (* set globals *)
  Common.set_time hdr.time;
  Param.set Params.nodes hdr.node_cnt;

  Log.log#log_notice "Simulation state read in:";
  Log.log#log_notice (sprintf "\t Nodes: %d" hdr.node_cnt);
  Log.log#log_notice (sprintf "\t Ntargets: %d" hdr.ntargets);
  Log.log#log_notice (sprintf "\t Time: %f" hdr.time);


  (* make node objects out of restored node states *)
  Nodes.set_nodes
    (Array.mapi
      (fun i nodestate -> 
	let db = (new NodeDB.nodeDB ~ntargets:hdr.ntargets) 
	and nd = (new Simplenode.simplenode 
	  ~pos_init:nodestate.Node.node_pos 
	  ~id:i
	  ~ntargets:hdr.ntargets
	) 
	in
	db#load_state nodestate.Node.db_state;
	nd#set_db db;
	(* bler_agent will register with node *)
	ignore (new Magic_bler_agent.magic_bler_agent nd);
	nd
      ) node_states);
  
  (* set up initial node position in internal structures of world object *)
  Nodes.iter (fun n -> (Gworld.world())#update_pos ~node:n ~oldpos_opt:None);
  assert ((Gworld.world())#neighbors_consistent);
)
*)
