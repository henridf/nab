(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf

class simplenode  ~pos_init ~id  : Node.node_t = 

object(s: #Node.node_t)
  
  inherit Log.loggable

  val mutable neighbors  = Common.NodeSet.empty
  val mutable pos = pos_init
  val mutable bler_agent = None

  val id = id
  val mutable db = new NodeDB.nodeDB
  val agents = Hashtbl.create 1

  val mutable recv_pkt_hooks = []
  val mutable app_send_pkt_hooks = []
  val mutable mhook = fun p a -> ()
   
  method pos = pos
  method id = id
  method x = Coord.xx pos
  method y = Coord.yy pos

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    objdescr <- (sprintf "/node/%d" id);

    let nmsg = (Naml_msg.mk_init_nodepos ~nid:s#id ~pos:pos_init) in
    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;

  )

  method move newpos = (
    let oldpos = pos in
    pos <- newpos;

    let nmsg = (Naml_msg.mk_node_move ~nid:s#id ~pos:newpos) in
    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;

    (* important to call update_pos *after* our own position has been updated *)
    (Gworld.world())#update_pos ~node:(s :> Node.node_t) ~oldpos_opt:(Some oldpos);
  )

  method add_neighbor n = (
    s#log_info (sprintf "Adding neighbor %d" n#id);
    assert (not (Common.NodeSet.mem n#id neighbors));
    db#add_encounter ~nid:n#id ~enc:(Common.enc ~time:(Common.get_time()) ~place:n#pos);
    neighbors <- Common.NodeSet.add n#id neighbors
  )

  method lose_neighbor n = (
    s#log_info (sprintf "Losing neighbor %d" n#id);
    assert (Common.NodeSet.mem n#id neighbors);
    neighbors <- Common.NodeSet.remove n#id neighbors
  )

  method is_neighbor n = (
    Common.NodeSet.mem n#id neighbors
  )

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
    let nmsg = (Naml_msg.mk_node_recv ~nid:s#id) in
    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;
    
    (* mhook called before shoving packet up the stack, because 
       it should not rely on any ordering *)
    mhook l2pkt (s :> Node.node_t);

    List.iter 
      (fun hook -> hook l2pkt.Packet.l3pkt)
      recv_pkt_hooks
  )
    
  method add_recv_pkt_hook  ~hook =
    recv_pkt_hooks <- recv_pkt_hooks @ [hook]
      
  method add_app_send_pkt_hook ~hook = 
    app_send_pkt_hooks <- app_send_pkt_hooks @ [hook]

  method add_mhook  ~hook =
    mhook <- hook
      

  method private send_pkt_ ~l3pkt ~dst = (
      (* this method only exists to factor code out of 
	 mac_send_pkt and cheat_send_pkt *)
    let nmsg = (Naml_msg.mk_node_send ~srcnid:s#id ~dstnid:dst#id) in
    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;

    let recvtime = Common.get_time() +. Mws_utils.propdelay pos dst#pos in
    let l2pkt = Packet.make_l2pkt ~srcid:id ~l2_dst:(Packet.L2_DST dst#id)
      ~l3pkt:l3pkt in

    mhook l2pkt (s :> Node.node_t);

    let recv_event() = dst#mac_recv_pkt ~l2pkt:l2pkt in
    (Gsched.sched())#sched_at ~handler:recv_event ~t:(Sched.Time recvtime)
  )

  method mac_send_pkt ~l3pkt ~mac_dst = (

      if not (s#is_neighbor mac_dst) then 
	s#log_error (Printf.sprintf "mac_send_pkt: %d not a neighbor. Dropping packet"
	  mac_dst#id)
      else
	s#send_pkt_ ~l3pkt:l3pkt ~dst:mac_dst
  )

  method cheat_send_pkt ~l3pkt ~dst = s#send_pkt_ ~l3pkt:l3pkt ~dst:dst

  method mac_bcast_pkt ~l3pkt = (

    let nmsg = (Naml_msg.mk_node_bcast ~nid:s#id) in
    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;

    let l2pkt = Packet.make_l2pkt ~srcid:id ~l2_dst:Packet.L2_BCAST
      ~l3pkt:l3pkt in

    mhook l2pkt (s :> Node.node_t);

    Common.NodeSet.iter (fun nid -> 
      let n = (Nodes.node(nid)) in
      let recvtime = Common.get_time() +. Mws_utils.propdelay pos n#pos in
      let recv_event() = n#mac_recv_pkt ~l2pkt:l2pkt in

      (Gsched.sched())#sched_at ~handler:recv_event ~t:(Sched.ASAP)
    ) neighbors
  )


  method dump_state ~node_cnt  = {
    Node.node_pos=s#pos;
    Node.db_state=db#dump_state ~node_cnt:node_cnt
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
