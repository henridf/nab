(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf

class simplenode  ~pos_init ~id  : Node.node_t = 

object(s)
  
  inherit Log.loggable

  val mutable neighbors  = Common.NodeSet.empty
  val mutable pos = pos_init
  val mutable bler_agent = None

  val id = id
  val mutable db = new NodeDB.nodeDB
  val agents = Hashtbl.create 1

  val mutable pkt_recv_hooks = []
   
  method pos = pos
  method id = id
  method x = Coord.xx pos
  method y = Coord.yy pos
  method private bler_agent = Misc.o2v bler_agent

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    objdescr <- (sprintf "/Node/%d" id);
    bler_agent <-  Some (new Bler_agent.bler_agent (s :> Node.node_t));

    let nmsg = (Naml_msg.mk_init_nodepos ~nid:s#id ~pos:pos_init) in
    s#logmsg_info nmsg;
    Trace.namltrace ~msg:nmsg;

    (Gworld.world())#update_pos ~node:(s :> Node.node_t) ~oldpos_opt:None
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

  method recv_pkt ~pkt = (
    s#bler_agent#recv pkt
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
