



(** 
  Null MAC Layer: A simple example of a class implementing the {!Mac.t}
  interface.
  
  "Null" meaning that there are no collisions, no losses, only transmission
  delay is applied.
*)


open Ether
open L2pkt
open Printf 

let bps = 1e7

class nullmac ?(stack=0) theowner : Mac.t = 
object(s)

  inherit Log.inheritable_loggable 
  inherit Mac_base.base ~stack ~bps theowner as super

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable)  "/nullmac";
  )

  method bps = bps

  method recv ?snr ~l2pkt () = (

    let dst = l2dst ~pkt:l2pkt in

    (* Throw away unicast packet if not for us, keep it otherwise *)
    begin match dst with
      | L2_BCAST ->  s#log_debug (lazy
	  (sprintf "Start RX, l2src %d, l2dst broadcast" (l2src ~pkt:l2pkt)));
	  s#accept_ l2pkt;

      | L2_DST d when (d = myid) ->  s#log_debug  (lazy
	  (sprintf "Start RX, l2src %d, l2dst %d" (l2src ~pkt:l2pkt) d));
	  s#accept_ l2pkt;

      | L2_DST d ->  s#log_debug  (lazy
	  (sprintf "Start RX, l2src %d, l2dst %d (not for us)" (l2src ~pkt:l2pkt) d));
    end

  )

  (* Called when we receive a packet which we keep (either unicast to us, or
     broadcast). *)
  method private accept_ l2pkt = (
    
    (* Compute delay to receive whole packet. Remember that recv() below is
       called at the very beginning of the packet reception, so we shouldn't
       hand this packet to upper layers until the whole thing has arrived. *)
    let t = Time.get_time() +. 
      super#xmitdelay ~bytes:(L2pkt.l2pkt_size ~l2pkt) in
    
    (* After the above delay, we will call send_up on our super class, which deals
       with pushing the packet into our node's protocol stack. *)
    let recv_event() =  super#send_up ~l2pkt in
    
    (Sched.s())#sched_at ~f:recv_event ~t:(Scheduler.Time t)	  
  )

  method xmit ~l2pkt = (
    s#log_debug (lazy "TX packet ");
    SimpleEther.emit ~stack ~nid:myid l2pkt
  )
end
      
    
    