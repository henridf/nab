(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** 
  Class nullmac : incoming and outgoing packets get passed in without any
  losses, only the transmission delayis applied.
*)


open Ether
open L2pkt

class nullmac owner : Mac.mac_t = 
object(s)

  inherit Log.inheritable_loggable

  val ownerid = owner#id
  val owner:#Simplenode.simplenode = owner

  initializer (
    s#set_objdescr ~owner:(owner :> #Log.inheritable_loggable)  "/nullmac";
  )

  method recv ?snr ~l2pkt () = 
    s#log_debug (lazy "RX packet ");
    let dst = l2dst ~pkt:l2pkt in
    if (dst = L2_BCAST || dst = L2_DST ownerid) then 
      let recvtime = 
	Common.get_time() 
	+. Mws_utils.xmitdelay ~bytes:(L2pkt.l2pkt_size ~l2pkt:l2pkt) in
      let recv_event() =  owner#mac_recv_pkt ~l2pkt in
      (Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time recvtime)

  method xmit ~l2pkt = (
    s#log_debug (lazy "TX packet ");
    Ether.emit ~nid:ownerid ~l2pkt
  )
end
      
    
    
