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
open Ether
open Mac_null
open Mac_contention

type node_state_t = Coord.coordf_t


exception Mac_Send_Failure

let coordmult = Coord.( ***. )

class simplenode id   = 

object(s)
  
  inherit Log.inheritable_loggable 

  val mutable recv_pkt_hooks = []
  val mutable recv_l2pkt_hooks = []
  val mutable app_send_pkt_hook = fun (pkt : L4pkt.l4pkt_t) ~(dst : Common.nodeid_t) -> ()
  val mutable pktin_mhooks = []
  val mutable pktout_mhooks = []
  val mutable mac = None

  val id = id

  method mac =o2v  mac
  method install_mac themac = mac <- Some themac
  method id = id

  initializer (
    s#set_objdescr (sprintf "/node/%d" id);
    s#log_debug (lazy (sprintf "New node %d" id));
    mac <- Some (new nullmac s)
  )


  method mac_recv_pkt ~l2pkt = (
    
    let l3pkt  = (L2pkt.l3pkt l2pkt) in

    s#log_debug (lazy (sprintf "Pkt received from source %d" 
      (L3pkt.l3src ~l3pkt:(L2pkt.l3pkt l2pkt))));

    (* mhook called before shoving packet up the stack, because 
       it should not rely on any ordering *)
    List.iter 
      (fun mhook -> mhook l2pkt s )
      pktin_mhooks;
    
    List.iter 
      (fun hook -> hook l3pkt)
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

    assert (L3pkt.l3ttl ~l3pkt:l3pkt >= 0);

    let l2pkt = L2pkt.make_l2pkt ~srcid:id ~l2_dst:(L2pkt.L2_DST dst#id)
      ~l3pkt:l3pkt in

    List.iter 
      (fun mhook -> mhook l2pkt s )
      pktout_mhooks;
    
    s#mac#xmit ~l2pkt
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

    assert (L3pkt.l3ttl ~l3pkt:l3pkt >= 0);

    let l2pkt = L2pkt.make_l2pkt ~srcid:id ~l2_dst:L2pkt.L2_BCAST
      ~l3pkt:l3pkt in

    List.iter 
    (fun mhook -> mhook l2pkt s )
      pktout_mhooks;

    s#mac#xmit ~l2pkt;
  )

  method set_trafficsource ~gen ~dst = 
    match gen() with
      | Some time_to_next_pkt ->
	  let next_pkt_event() = s#trafficsource gen dst in
	  (Gsched.sched())#sched_in ~f:next_pkt_event ~t:time_to_next_pkt
      | None -> ()
	  
  method private trafficsource gen dst = 
    s#originate_app_pkt ~dst;
    (* when gen() returns None, this trafficsource is done sending *)
    match gen() with
      | Some time_to_next_pkt ->
	  let next_pkt_event() = s#trafficsource gen dst in
	  (Gsched.sched())#sched_in ~f:next_pkt_event ~t:time_to_next_pkt
      | None -> ()
      
  method originate_app_pkt ~dst = 
    app_send_pkt_hook `APP_PKT ~dst

  method dump_state = (Gworld.world())#nodepos id


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
