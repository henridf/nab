(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Simplenode: Encapsulates most of the state and constituent objects used to
  model a node.
  @author Henri Dubois-Ferriere. *)

exception Mac_Send_Failure
  (** Exception raised when trying to unicast a packet to a non-neighboring
    node. *)


type node_state_t = Coord.coordf_t
    (** this type is used for storing node state to file *)
    
    
class simplenode : Common.nodeid_t ->
  
object ('a)
  
  method dump_state : node_state_t 
    
  method objdescr : string
    
  method id : Common.nodeid_t

  method install_mac : ?stack:int -> Mac.mac_t -> unit
    (** Sets the node's {!Mac.mac_t} object. If a Mac was already in place it 
      is  squished. *)

  method mac : ?stack:int -> unit -> Mac.mac_t
    (** Returns the node's {!Mac.mac_t} object. *)

  method mac_recv_pkt : ?stack:int -> L2pkt.t -> unit

  method add_rt_agent : ?stack:int -> Rt_agent_base.t -> unit

  method mac_send_pkt : 
    ?stack:int -> 
    l3pkt:L3pkt.t -> 
    dstid:Common.nodeid_t -> 
    unit -> unit
    (** Sent packet to neighbor mac_dst. Raises {!Simplenode.Mac_Send_Failure} if mac_dst is not a neighbor *)

  method cheat_send_pkt : ?stack:int -> l3pkt:L3pkt.t -> Common.nodeid_t -> unit
    (** Same as above except will accept sending pkt to any destination *)

  method mac_bcast_pkt : ?stack:int -> L3pkt.t -> unit
    (** Broadcast packet to all neighbors. *)

  method add_pktin_mhook : hook:(L2pkt.t -> 'a -> unit) -> unit
    (** Any monitoring application can register here to see all packets entering
      the node.
      If multiple apps, order in which called is unspecified.*)
    
  method add_pktout_mhook : hook:(L2pkt.t -> 'a -> unit) -> unit
    (** Any monitoring application can register here to see all packets leaving
      the node.
      If multiple apps, order in which called is unspecified.*)
    
  method clear_pkt_mhooks :  unit 
    (** clears pktin and pktout mhooks *)

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
    
    
