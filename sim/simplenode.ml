(* Mar03
   wierd: decrementing shopcount when packet not sent seems necessary, 
   ie omission was a bug, but not sure if it changes anything.
   anyway current solution is a bit of a quick hack 
   May03
   maybe this is due to the fact that ttl does not appear to be corrected
   either - should be looked into.
*)


(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc

type node_state_t = {
  node_pos : Coord.coordf_t;
}

exception Mac_Send_Failure
exception Mac_Bcast_Failure

let coordmult = Coord.( ***. )

class simplenode  ~id   = 

object(s)
  
  inherit Log.loggable

  val id = id

  val mutable recv_pkt_hooks = []
  val mutable recv_l2pkt_hooks = []
  val mutable app_send_pkt_hook = fun pkt ~(dst : Common.nodeid_t) -> ()
  val mutable pktin_mhooks = []
  val mutable pktout_mhooks = []
   
  method id = id

  initializer (
    objdescr <- (sprintf "/node/%d " id);
    s#log_debug (lazy (sprintf "New node %d" id));
  )


  method mac_recv_pkt ~l2pkt = (
    
    (* mhook called before shoving packet up the stack, because 
       it should not rely on any ordering *)
    List.iter 
    (fun mhook -> mhook l2pkt s )
      pktin_mhooks;

    List.iter 
      (fun hook -> hook l2pkt.Packet.l3pkt)
      recv_pkt_hooks;

    List.iter 
      (fun hook -> hook l2pkt)
      recv_l2pkt_hooks
  )
    

  method add_recv_pkt_hook ~hook =
    recv_pkt_hooks <- recv_pkt_hooks @ [hook]
      
  method add_recv_l2pkt_hook  ~hook =
    recv_l2pkt_hooks <- recv_l2pkt_hooks @ [hook]
      
  method add_app_send_pkt_hook ~hook = 
    app_send_pkt_hook <- hook

  method add_pktin_mhook  ~hook =
    pktin_mhooks <- hook::pktin_mhooks
      
  method add_pktout_mhook  ~hook =
    pktout_mhooks <- hook::pktout_mhooks
      
  method clear_pkt_mhooks = (
    pktout_mhooks <- [];
    pktin_mhooks <- []
  )

  method private send_pkt_ ~l3pkt ~dstid = (
    (* this method only exists to factor code out of 
       mac_send_pkt and cheat_send_pkt *)

    let dst = (Nodes.node(dstid)) in

    assert (Packet.get_l3ttl ~l3pkt:l3pkt >= 0);

    let l2pkt = Packet.make_l2pkt ~srcid:id ~l2_dst:(Packet.L2_DST dst#id)
      ~l3pkt:l3pkt in

    let delay = 
      Mws_utils.xmitdelay ~bytes:(Packet.l2pkt_size ~l2pkt:l2pkt)
      +. Mws_utils.propdelay 
	((Gworld.world())#nodepos id) 
	((Gworld.world())#nodepos dstid) in
    let recvtime = Common.get_time() +. delay in

    List.iter 
    (fun mhook -> mhook l2pkt s )
      pktout_mhooks;

    let recv_event() = dst#mac_recv_pkt ~l2pkt:l2pkt in
    (Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time recvtime);
  )

  method mac_send_pkt ~l3pkt ~dstid = (
    if not ((Gworld.world())#are_neighbors s#id dstid) then (
	s#log_notice (lazy (Printf.sprintf "mac_send_pkt: %d not a neighbor." dstid));
	raise Mac_Send_Failure
      ) else
	s#send_pkt_ ~l3pkt:l3pkt ~dstid:dstid
  )
    
  method cheat_send_pkt ~l3pkt ~dstid = s#send_pkt_ ~l3pkt:l3pkt ~dstid:dstid

  method mac_bcast_pkt ~l3pkt = (

    assert (Packet.get_l3ttl ~l3pkt:l3pkt >= 0);

    let l2pkt = Packet.make_l2pkt ~srcid:id ~l2_dst:Packet.L2_BCAST
      ~l3pkt:l3pkt in

    List.iter 
    (fun mhook -> mhook l2pkt s )
      pktout_mhooks;

    let neighbors = (Gworld.world())#neighbors id in

    List.iter (fun nid -> 
      let n = (Nodes.node(nid)) in
    let recvtime = 
      Common.get_time()
      +. Mws_utils.xmitdelay ~bytes:(Packet.l2pkt_size ~l2pkt:l2pkt)
      +. Mws_utils.propdelay 
	((Gworld.world())#nodepos id) 
	((Gworld.world())#nodepos nid) in
      let recv_event() = 
	n#mac_recv_pkt ~l2pkt:(Packet.clone_l2pkt ~l2pkt:l2pkt) in
      (Gsched.sched())#sched_at ~f:recv_event ~t:(Sched.Time recvtime)
    ) neighbors
  )

  method trafficsource ~dstid ~pkts_per_sec = 
    s#originate_app_pkt ~dstid:dstid;
    let time_to_next_pkt = 1.0 /. (i2f pkts_per_sec) in
    let next_pkt_event() = 
      s#trafficsource ~dstid:dstid ~pkts_per_sec:pkts_per_sec     in
    (Gsched.sched())#sched_in ~f:next_pkt_event ~t:time_to_next_pkt
      

  method originate_app_pkt ~dstid = 
    app_send_pkt_hook Packet.APP_PLD ~dst:dstid

  method dump_state = {
    node_pos=(Gworld.world())#nodepos id
  } 

end









(*
method next_position ~node ~mob = (
    match mob with
      | RANDOMWALK -> 
	  s#reflect_ (
	    node#pos +++. ([|Random.float 2.0; Random.float 2.0|] ---. [|1.0; 1.0|])
	  )
      | WAYPOINT -> raise Misc.Not_Implemented
  )
  *)
