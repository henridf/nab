(* wierd: decrementing shopcount when packet not send seems necessary, 
   ie omission was a bug, but not sure if it changes anything.
   anyway current solution is a bit of a quick hack *)

(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc

exception Mac_Send_Failure
exception Mac_Bcast_Failure

let coordmult = Coord.( ***. )

class simplenode  ~pos_init ~id ~ntargets : Node.node_t = 

object(s: #Node.node_t)
  
  inherit Log.loggable

  val mutable neighbors  = []
  val mutable pos = pos_init
  val mutable bler_agent = None
  val mutable mob_getnewpos = fun ~node -> (0.0,0.0)
  val mutable speed = 0.0

  val id = id

  val agents = Hashtbl.create 1

  val mutable recv_pkt_hooks = []
  val mutable recv_l2pkt_hooks = []
  val mutable app_send_pkt_hook = fun pkt ~dst -> ()
  val mutable control_hook = fun p -> ()
  val mutable mhook = fun p a -> ()
   
  method pos = pos
  method id = id
  method x = Coord.xx pos
  method y = Coord.yy pos

  initializer (
    objdescr <- (sprintf "/node/%d" id);

    let nmsg = (Naml_msg.mk_init_nodepos ~nid:s#id ~pos:pos_init) in
    s#logmsg_debug nmsg;
    Trace.namltrace ~msg:nmsg;
  )

  method move newpos = (
    let oldpos = pos in
    pos <- newpos;
    
    let nmsg = (Naml_msg.mk_node_move ~nid:s#id ~pos:newpos) in
    s#logmsg_debug nmsg;
    Trace.namltrace ~msg:nmsg;

    (* important to call update_pos *after* our own position has been updated *)
    (Gworld.world())#update_pos ~node:(s :> Node.node_t) ~oldpos_opt:(Some oldpos);
  )

  method setmob themob = mob_getnewpos <- themob
  method set_speed_mps thespeed = speed <- thespeed

  method selfmove  = (

    let newpos = mob_getnewpos ~node:(s :> Node.node_t) in
    s#move newpos;
    
    (* mob is assumed to move us by one meter, so we should schedule the next
       one in 1 / speed_mps seconds *)
    let move_event() = s#selfmove in
    (Gsched.sched())#sched_in ~handler:move_event ~t:(1.0/.speed)
  )

  method add_neighbor n = (
    assert (not (List.mem n#id neighbors));
    if n#id < ntargets then (
(*      db#add_encounter ~nid:n#id ~enc:(Common.enc ~time:(Common.get_time()) ~place:n#pos);*)
    );
    neighbors <- n#id::neighbors
  )

  method lose_neighbor n = (
    assert (List.mem n#id neighbors);
    neighbors <- Misc.list_without neighbors n#id
  )

  method is_neighbor n = List.mem n#id neighbors


  method private bind_agent ~agent ~port = 
    match (Hashtbl.mem agents port) with
      | false -> Hashtbl.add agents agent
      | true -> raise 
	  (Failure 
	    (sprintf "Node %d Cannot bind_agent on port %d: already busy\n" 
	      s#id 
	      port))

  method private lookup_agent  ~port = 
    try 
      Some (Hashtbl.find agents port) 
    with
	Not_found -> None
	
  method neighbors = neighbors

  method mac_recv_pkt ~l2pkt = (
    let nmsg = 
      (Naml_msg.mk_node_recv 
	~src:s#id 
	~sender:(Packet.get_l2src ~pkt:l2pkt)) in

    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;
    
    (* mhook called before shoving packet up the stack, because 
       it should not rely on any ordering *)
    mhook l2pkt (s :> Node.node_t);

    List.iter 
      (fun hook -> hook l2pkt.Packet.l3pkt)
      recv_pkt_hooks;

    List.iter 
      (fun hook -> hook l2pkt)
      recv_l2pkt_hooks
  )
    
  method add_recv_pkt_hook  ~hook =
    recv_pkt_hooks <- recv_pkt_hooks @ [hook]
      
  method add_recv_l2pkt_hook  ~hook =
    recv_l2pkt_hooks <- recv_l2pkt_hooks @ [hook]
      
  method add_app_send_pkt_hook ~hook = 
    app_send_pkt_hook <- hook

  method add_control_hook ~hook = 
    control_hook <- hook

  method add_mhook  ~hook =
    mhook <- hook
      
  method agent_control ~action = control_hook action

  method private send_pkt_ ~l3pkt ~dstid = (
    let dst = (Nodes.node(dstid)) in
      (* this method only exists to factor code out of 
	 mac_send_pkt and cheat_send_pkt *)
    let nmsg = (Naml_msg.mk_node_send ~srcnid:s#id ~dstnid:dstid) in
    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;
    assert (Packet.get_l3ttl ~l3pkt:l3pkt >= 0);

    let l2pkt = Packet.make_l2pkt ~srcid:id ~l2_dst:(Packet.L2_DST dst#id)
      ~l3pkt:l3pkt in

    let delay = 
      Mws_utils.xmitdelay ~bytes:(Packet.l2pkt_size ~l2pkt:l2pkt)
      +. Mws_utils.propdelay pos dst#pos in
    let recvtime = Common.get_time() +. delay in

    mhook l2pkt (s :> Node.node_t);

    let recv_event() = dst#mac_recv_pkt ~l2pkt:l2pkt in
    (Gsched.sched())#sched_at ~handler:recv_event ~t:(Sched.Time recvtime)
  )

  method mac_send_pkt ~l3pkt ~dstid = (
    let dst = (Nodes.node(dstid)) in
      if not (s#is_neighbor dst) then (
(*	s#log_notice (Printf.sprintf "mac_send_pkt: %d not a neighbor." dstid);*)
	let l3hdr = Packet.get_l3hdr l3pkt in
	l3hdr.Packet.grep_shopcount <- l3hdr.Packet.grep_shopcount - 1;
	raise Mac_Send_Failure
      ) else
	s#send_pkt_ ~l3pkt:l3pkt ~dstid:dstid
  )
    
  method cheat_send_pkt ~l3pkt ~dstid = s#send_pkt_ ~l3pkt:l3pkt ~dstid:dstid

  method mac_bcast_pkt ~l3pkt = (

    let nmsg = (Naml_msg.mk_node_bcast ~nid:s#id ) in
    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;

    assert (Packet.get_l3ttl ~l3pkt:l3pkt >= 0);

    let l2pkt = Packet.make_l2pkt ~srcid:id ~l2_dst:Packet.L2_BCAST
      ~l3pkt:l3pkt in
    mhook l2pkt (s :> Node.node_t);

    if (List.length neighbors = 0) then (
      let l3hdr = Packet.get_l3hdr l3pkt in
      l3hdr.Packet.grep_shopcount <- l3hdr.Packet.grep_shopcount - 1;
(*      raise Mac_Bcast_Failure;*)
    );
    List.iter (fun nid -> 
      let n = (Nodes.node(nid)) in
    let recvtime = 
      Common.get_time()
      +. Mws_utils.xmitdelay ~bytes:(Packet.l2pkt_size ~l2pkt:l2pkt)
      +. Mws_utils.propdelay pos n#pos in
      let recv_event() = 
	n#mac_recv_pkt ~l2pkt:(Packet.clone_l2pkt ~l2pkt:l2pkt) in
      (Gsched.sched())#sched_at ~handler:recv_event ~t:(Sched.Time recvtime)
    ) neighbors
  )


  method trafficsource ~dstid ~pkts_per_sec = 
    s#originate_app_pkt ~dstid:dstid;
    let time_to_next_pkt = 1.0 /. (i2f pkts_per_sec) in
    let next_pkt_event() = 
      s#trafficsource ~dstid:dstid ~pkts_per_sec:pkts_per_sec     in
    (Gsched.sched())#sched_in ~handler:next_pkt_event ~t:time_to_next_pkt
      

  method originate_app_pkt ~dstid = 
    app_send_pkt_hook Packet.APP_PLD ~dst:dstid

  method dump_state = {
    Node.node_pos=s#pos;
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
