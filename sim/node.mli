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
    
  val mutable neighbors : Common.nodeid_t list
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
  method neighbors : Common.nodeid_t list
    
  method move : Coord.coordf_t -> unit

  method mac_recv_pkt : l2pkt:Packet.l2packet_t -> unit
  method mac_send_pkt : l3pkt:Packet.packet_t -> mac_dst:node_t -> unit
    (* Sent packet to neighbor mac_dst. May raise Failure if mac_dst is not a neighbor *)

  method cheat_send_pkt : l3pkt:Packet.packet_t -> dst:node_t -> unit
    (* Same as above except will accept sending pkt to any destination *)

  method mac_bcast_pkt : l3pkt:Packet.packet_t -> unit
    (* Broadcast packet to all neighbors *)

  method add_recv_pkt_hook : hook:(Packet.packet_t -> unit) -> unit
    (* Any agent (routing, application, etc) on this node who might
       receive packets should register through this hook. 
       Agent can filter based on l3hdr contents.
    *)

  method add_mhook : hook:(Packet.l2packet_t -> node_t -> unit) -> unit
    
  method add_app_send_pkt_hook : hook:(Packet.app_packet_t -> unit) -> unit
    (* Routing/Protocol agents should hook here to touch packets being sent by 
       applications. 
       For example a proactive routing agent would simply look up the next
       hop, then call mac_send_pkt on this node.
       Or an on-demand routing agent would put the packet aside and flood its
       RREQ via mac_bcast_pkt, and then forward the packet via mac_send_pkt
       once the route is established *)

  method originate_app_pkt : dst:node_t -> unit
    (* originates a packet from an application on this node to dst:
       create the packet and shove it down the app_send_pkt_hooks *)


  method dump_state : node_state_t 
end
  
  
	  
(*
class node : 
  pos_init:Coord.coordf_t -> 
  id:Common.nodeid_t ->
  node_t
*)
  
  
