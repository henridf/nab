(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


(** 
  Ether: The shared medium onto which nodes transmit. 
  So far, we have a simple model with no propagation effects.
*)

open Misc 

let speed_of_light = 1e8
let bits_per_sec = 1e4
let propdelay p1 p2 = (sqrt (Coord.dist_sq p1 p2)) /. speed_of_light
let xmitdelay ~bytes = (i2f (bytes * 8)) /. bits_per_sec


module type Ether_t = 
sig
  val emit : nid:Common.nodeid_t -> l2pkt:L2pkt.l2packet_t -> unit
    (** A node's MAC calls this to emit bits into the air. The Ether module
      then takes care of sending them, with appropriate propagation delay and SNR,
      to nodes within range. *)
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
	+. propdelay 
	  ((Gworld.world())#nodepos id)
	  ((Gworld.world())#nodepos nid) in
      let recv_event() = 
	n#mac#recv ~l2pkt:(L2pkt.clone_l2pkt ~l2pkt:l2pkt) () in
      (Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time recvtime)
      )
    ) neighbors

end
