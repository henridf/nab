(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Simplenode: Encapsulates most of the state and constituent objects used to
  model a node.
  @author Henri Dubois-Ferriere. *)

exception Mac_Send_Failure

type node_state_t = Coord.coordf_t
    (** this type is used for storing node state to file *)
    
    
class simplenode : Common.nodeid_t ->
  
object ('a)
  
  method dump_state : node_state_t 
    
  method objdescr : string
    
  method id : Common.nodeid_t

  method install_mac : Mac.mac_t -> unit
    (** Sets the nodes Mac object. If a Mac was already in place it 
      is  squished. *)

  method mac : Mac.mac_t
    (** Returns the node Mac object. *)

  method mac_recv_pkt : l2pkt:L2pkt.l2packet_t -> unit
  method mac_send_pkt : l3pkt:L3pkt.l3packet_t -> dstid:Common.nodeid_t -> unit
    (** Sent packet to neighbor mac_dst. Should raise Mac_Send_Failure if mac_dst is not a neighbor *)

  method cheat_send_pkt : l3pkt:L3pkt.l3packet_t -> dstid:Common.nodeid_t -> unit
    (** Same as above except will accept sending pkt to any destination *)

  method mac_bcast_pkt : l3pkt:L3pkt.l3packet_t -> unit
    (** Broadcast packet to all neighbors. *)

  method add_recv_pkt_hook : hook:(L3pkt.l3packet_t -> unit) -> unit
    (** Any agent (routing, application, etc) on this node who might
      receive packets should register through this hook. 
      Agent can filter based on l3hdr contents. *)

  method add_recv_l2pkt_hook : hook:(L2pkt.l2packet_t -> unit) -> unit
    (** Same as add_recv_l3pkt_hook, except the packet comes with L2 hdr still on, for 
      those agents which might need it. *)

  method add_pktin_mhook : hook:(L2pkt.l2packet_t -> 'a -> unit) -> unit
    (** Any monitoring application can register here to see all packets entering
      the node.
      If multiple apps, order in which called is unspecified.*)
    
    
  method add_pktout_mhook : hook:(L2pkt.l2packet_t -> 'a -> unit) -> unit
    (** Any monitoring application can register here to see all packets leaving
      the node.
      If multiple apps, order in which called is unspecified.*)
    
  method clear_pkt_mhooks :  unit 
    (** clears pktin and pktout mhooks *)

  method add_app_send_pkt_hook : hook:(L4pkt.l4pkt_t -> dst:Common.nodeid_t -> unit) -> unit
    (** Routing/Protocol agents should hook here to touch packets being sent by 
      applications. 

      For example a proactive routing agent would simply look up the next
      hop, then call mac_send_pkt on this node.
      Or an on-demand routing agent would put the packet aside and flood its
      RREQ via mac_bcast_pkt, and then forward the packet via mac_send_pkt
      once the route is established 

      This squishes the current hook if any already in place *)


  method originate_app_pkt : dst:Common.nodeid_t -> unit
    (** originates a packet from an application on this node to dst:
      create the packet and shove it down the app_send_pkt_hooks *)

  method set_trafficsource :  gen:Tsource.traffic_generator_t ->  dst:Common.nodeid_t -> unit
    (** Installs a traffic source and uses it to generate application packets
      to node dst. Gen is a function which when called returns  either 
      - Some t, (where t is time until sending next packet) or
      - None, meaning that there are no more packets to send 
      
      Multiple trafficsources to multiple destinations can be installed (XXX not
      tested though)
    *)
   
end
  
(**/**)  
    
(*
  class node : 
  pos_init:Coord.coordf_t -> 
  id:Common.nodeid_t ->
  node_t
*)
    
    
