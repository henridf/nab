(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  Null MAC Layer: no losses, no collisions, only transmission delay is applied.
*)


open Ether
open L2pkt
open Printf 

class nullmac owner : Mac.mac_t = 
object(s)

  inherit Log.inheritable_loggable

  val ownerid = owner#id
  val owner:#Simplenode.simplenode = owner

  initializer (
    s#set_objdescr ~owner:(owner :> #Log.inheritable_loggable)  "/nullmac"
  )

  method recv ?snr ~l2pkt () = (
    let dst = l2dst ~pkt:l2pkt in

    match dst with
      | L2_BCAST ->
	  s#log_debug (lazy
	    (sprintf "Start RX, l2src %d, l2dst broadcast" (l2src ~pkt:l2pkt)));
	  let recvtime = 
	    Common.get_time() 
	    +. xmitdelay ~bytes:(L2pkt.l2pkt_size ~l2pkt:l2pkt) in
	  let recv_event() =  owner#mac_recv_pkt ~l2pkt in
	  (Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time recvtime)
      | L2_DST d when (d = ownerid) ->
	  s#log_debug  (lazy
	    (sprintf "Start RX, l2src %d, l2dst %d" (l2src ~pkt:l2pkt) d));
	  let recvtime = 
	    Common.get_time() 
	    +. xmitdelay ~bytes:(L2pkt.l2pkt_size ~l2pkt:l2pkt) in
	  let recv_event() =  owner#mac_recv_pkt ~l2pkt in
	  (Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time recvtime)	  
      | L2_DST d ->
	  s#log_debug  (lazy
	    (sprintf "Start RX, l2src %d, l2dst %d (not for us)" (l2src ~pkt:l2pkt) d));
  )

  method xmit ~l2pkt = (
    s#log_debug (lazy "TX packet ");
    SimpleEther.emit ~nid:ownerid ~l2pkt
  )
end
      
    
    
