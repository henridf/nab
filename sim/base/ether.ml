(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)








open Misc 


let speed_of_light = 1e8
let propdelay p1 p2 = (sqrt (Coord.dist_sq p1 p2)) /. speed_of_light


module type Ether_t = sig 
  val emit : stack:int -> nid:Common.nodeid_t -> L2pkt.t -> unit 
end


module SimpleEther : Ether_t = 
struct 
  let emit ~stack ~nid l2pkt = 
    let neighbors = (World.w())#neighbors nid in
    
    List.iter (fun id -> 
      if id <> nid then (
	let n = (Nodes.node(id)) in
	let recvtime = 
	  Time.get_time()
	  +. propdelay 
	  ((World.w())#nodepos id)
	    ((World.w())#nodepos nid) in
	let recv_event() = 
	  (n#mac ~stack ())#recv ~l2pkt:(L2pkt.clone_l2pkt ~l2pkt:l2pkt) () in
	(Sched.s())#sched_at ~f:recv_event ~t:(Scheduler.Time recvtime)
      )
    ) neighbors

end


module NullEther : Ether_t = 
struct 
  let emit ~stack ~nid l2pkt = 
    let l2dst = L2pkt.l2dst l2pkt in
    let neighbors = (World.w())#neighbors nid in
    
    List.iter (fun id -> 
      if id <> nid && (l2dst = L2pkt.l2_bcast_addr || l2dst = id)
	
      then (
	let n = (Nodes.node(id)) in
	let recvtime = 
	  Time.get_time()
	  +. propdelay 
	  ((World.w())#nodepos id)
	    ((World.w())#nodepos nid) in
	let recv_event() = 
	  (n#mac ~stack ())#recv ~l2pkt:(L2pkt.clone_l2pkt ~l2pkt:l2pkt) () in
	(Sched.s())#sched_at ~f:recv_event ~t:(Scheduler.Time recvtime)
      )
    ) neighbors

end
