(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


open Misc 


let speed_of_light = 1e8
let propdelay p1 p2 = (sqrt (Coord.dist_sq p1 p2)) /. speed_of_light


module type Ether_t = sig 
  val emit : ?stack:int -> nid:Common.nodeid_t -> L2pkt.t -> unit 
end


module SimpleEther : Ether_t = 
struct 
  let emit ?(stack=0) ~nid l2pkt = 
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
	  (n#mac ~stack ())#recv ~l2pkt:(L2pkt.clone_l2pkt ~l2pkt:l2pkt) () in
	(Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time recvtime)
      )
    ) neighbors

end
