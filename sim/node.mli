(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* this type is used for storing to file *)
type node_state_t = {
  node_pos : Coord.coordf_t;
  db_state : NodeDB.nodeDB_state_t
}




class type node_t =
	  
object
  val id : Common.nodeid_t

  val mutable neighbors : Common.NodeSet.t
  val mutable pos : Coord.coordf_t

  method objdescr : string

  method id : Common.nodeid_t
  method pos : Coord.coordf_t
  method x : float
  method y : float
    
  method db : NodeDB.nodeDB_t
  method set_db : NodeDB.nodeDB_t -> unit
    
  method add_neighbor : node_t -> unit
  method lose_neighbor : node_t -> unit
  method is_neighbor : node_t -> bool
  method neighbors : Common.NodeSet.t
    
  method move : Coord.coordf_t -> unit
  method recv_pkt : pkt:Packet.packet_t -> unit

  method dump_state : node_cnt:int -> node_state_t 
end
  
  
	  
(*
class node : 
  pos_init:Coord.coordf_t -> 
  id:Common.nodeid_t ->
  node_t
*)
  
  
