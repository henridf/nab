(** 
  "Cheat" MAC Layer: A MAC layer which does not take into account connectivity
  range or neighborhood. Using the "cheat" MAC, one can directly send a packet
  to any node, anywhere in the network. 
  This behavior only applies to unicast packets; broadcast packets are
  received only by nodes within connectivity range.

  This MAC layer also does not model any collisions or losses; only
  transmission delay is applied.
*)


open Ether
open L2pkt
open Printf 

class cheatmac ?(stack=0) theowner : Mac.t = 
object(s)


  inherit Mac_null.nullmac ~stack theowner as super
  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/cheatmac";
  )

  method xmit ~l2pkt = (
    s#log_debug (lazy "TX packet ");
    
    match L2pkt.l2dst ~pkt:l2pkt with 
      | L2pkt.L2_BCAST ->
	  SimpleEther.emit ~stack ~nid:theowner#id l2pkt
      | L2pkt.L2_DST dstid ->
	  let dstnode = (Nodes.node(dstid)) in
	  let recvtime = 
	    Time.get_time()
	    +. propdelay 
	      ((World.w())#nodepos dstid)
	      ((World.w())#nodepos theowner#id) in
	  let recv_event() = 
	    (dstnode#mac ~stack ())#recv ~l2pkt:(L2pkt.clone_l2pkt ~l2pkt:l2pkt) () in
	  (Sched.s())#sched_at ~f:recv_event ~t:(Scheduler.Time recvtime)
  )
end 
      
    
    
