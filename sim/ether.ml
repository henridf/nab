(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  Ether: The shared medium onto which nodes transmit. 
  So far, we have a simple model with no propagation effects.
*)

module type Ether_t = 
sig
  val emit : nid:Common.nodeid_t -> l2pkt:L2pkt.l2packet_t -> unit
    (** A node's MAC calls this to emit bits into the air. The Ether module
      then takes care of sending them, with appropriate propagation delay,
      to nodes within range. This is also where the SNR and other propagation
      effects will be computed. *)
end


module Ether : Ether_t = 
struct 
  let emit ~nid ~l2pkt = 
    let neighbors = (Gworld.world())#neighbors nid in
    
    List.iter (fun id -> 
      if id <> nid then (
      let n = (Nodes.node(id)) in
      let recvtime = 
	Common.get_time()
	+. Mws_utils.propdelay 
	  ((Gworld.world())#nodepos id)
	  ((Gworld.world())#nodepos nid) in
      let recv_event() = 
	n#mac#recv ~l2pkt:(L2pkt.clone_l2pkt ~l2pkt:l2pkt) () in
      (Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time recvtime)
      )
    ) neighbors

end
