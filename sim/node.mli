(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* this type is used for storing to file *)
type node_state_t = {
  node_pos : Coord.coordf_t;
}
    
type agent_ctrl_t = AGENT_STOP | AGENT_START

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

  method add_neighbor : node_t -> unit
  method lose_neighbor : node_t -> unit
  method is_neighbor : node_t -> bool
  method neighbors : Common.nodeid_t list
    
  method setmob : (node:node_t -> Coord.coordf_t) -> unit
  method set_speed_mps : float -> unit
  method selfmove : unit
  method move : Coord.coordf_t -> unit

  method mac_recv_pkt : l2pkt:Packet.l2packet_t -> unit
  method mac_send_pkt : l3pkt:Packet.l3packet_t -> dstid:Common.nodeid_t -> unit
    (* Sent packet to neighbor mac_dst. Should raise Mac_Send_Failure if mac_dst is not a neighbor *)

  method cheat_send_pkt : l3pkt:Packet.l3packet_t -> dstid:Common.nodeid_t -> unit
    (* Same as above except will accept sending pkt to any destination *)

  method mac_bcast_pkt : l3pkt:Packet.l3packet_t -> unit
    (* Broadcast packet to all neighbors. Should raise Mac_Bcast_Failure if
       node has no neighbors *)

  method add_recv_pkt_hook : hook:(Packet.l3packet_t -> unit) -> unit
    (* Any agent (routing, application, etc) on this node who might
       receive packets should register through this hook. 
       Agent can filter based on l3hdr contents.
    *)

  method add_recv_l2pkt_hook : hook:(Packet.l2packet_t -> unit) -> unit
    (* Same as add_recv_l3pkt_hook, except the packet comes with L2 hdr still on, for 
       those agents which might need it.
    *)

  method add_mhook : hook:(Packet.l2packet_t -> node_t -> unit) -> unit
    (* the mhook (monitoring hook) on a node gets called each time a packet
       leaves or enters the node. 
       This squishes the current mhook if any already in place *)
    
  method add_control_hook : hook:(agent_ctrl_t -> unit) -> unit
  method agent_control : action:agent_ctrl_t -> unit

  method add_app_send_pkt_hook : hook:(Packet.l4pld_t -> dst:Common.nodeid_t -> unit) -> unit
    (* Routing/Protocol agents should hook here to touch packets being sent by 
       applications. 

       For example a proactive routing agent would simply look up the next
       hop, then call mac_send_pkt on this node.
       Or an on-demand routing agent would put the packet aside and flood its
       RREQ via mac_bcast_pkt, and then forward the packet via mac_send_pkt
       once the route is established 

       This squishes the current hook if any already in place *)



  method originate_app_pkt : dstid:Common.nodeid_t -> unit
    (* originates a packet from an application on this node to dst:
       create the packet and shove it down the app_send_pkt_hooks *)

  method trafficsource  : 
    dstid:Common.nodeid_t -> 
    pkts_per_sec:int -> unit

  method dump_state : node_state_t 
end
  
  
	  
(*
class node : 
  pos_init:Coord.coordf_t -> 
  id:Common.nodeid_t ->
  node_t
*)
  
  
