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
  time:Common.time_t
}

type sim_state_t = Node.node_state_t array (* size Nodes_cnt * Nodes_cnt *)
  
let save_state ~node_cnt ~out_chan = (
  let descr = sprintf "mws datafile\n Parameters:\n nodes: %d\n time: %f\n" 
    node_cnt 
    (Common.get_time())
  in
  let state_hdr = {
    node_cnt=node_cnt;
    time=(Common.get_time())
  }
  in
  let (enc_array:sim_state_t) = 
    Nodes.map (fun n -> n#dump_state ~node_cnt:node_cnt)
  in

  if (node_cnt <> Array.length enc_array) then 
    raise (Failure 
      "Persistency.save_state: node_cnt was different than length of Nodes array");
  
  Marshal.to_channel out_chan descr [];
  Marshal.to_channel out_chan state_hdr [];
  Marshal.to_channel out_chan enc_array [];

  Log.log#log_info "Simulation state saved.";

  close_out out_chan
)


let read_state ~in_chan = (

  (* get bits from channel *)
  let str = (Marshal.from_channel in_chan : string) in
  let hdr = (Marshal.from_channel in_chan : state_hdr_t) in
  let enc_array = (Marshal.from_channel in_chan : sim_state_t) in
  
  if (hdr.node_cnt <> Array.length enc_array) then 
    raise (Failure 
      "Persistency.read_state: node_cnt was different than length of Nodes array");
  
  (* set globals *)
  Common.set_time hdr.time;
  Param.set Params.nodes hdr.node_cnt;

  Log.log#log_info "Simulation state read in:";
  Log.log#log_info (sprintf "\t Nodes: %d" hdr.node_cnt);
  Log.log#log_info (sprintf "\t Time: %f" hdr.time);

  (* make node objects out of restored node states *)
  Nodes.set_nodes
    (Array.mapi
      (fun i nodestate -> 
	let db = (new NodeDB.nodeDB) 
	and nd = (new Simplenode.simplenode ~pos_init:nodestate.Node.node_pos ~id:i) 
	in
	db#load_state nodestate.Node.db_state;
	nd#set_db db;
	nd
      ) enc_array);
  
  (* Nodes call update_pos in their constructor, but when they do so the
     nodes_array in Nodes is still empty so we need to call update_pos
     explicitly after *)
  Nodes.iter (fun n -> (Gworld.world())#update_pos ~node:n ~oldpos_opt:None);
  assert ((Gworld.world())#neighbors_consistent);

)
