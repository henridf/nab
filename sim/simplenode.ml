(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf

type node_state_t = {node_pos : int}

class type node_t =

object
  val id : int

  val mutable neighbors : Common.NodeSet.t
  val mutable pos : Coord.coordf_t

  method objdescr : string

  method id : Common.nodeid_t
  method pos : Coord.coordf_t
  method x : float
  method y : float

  method db : NodeDB.nodeDB_t
  method set_db : NodeDB.nodeDB_t -> unit

  method add_neighbor : node_t -> unit
  method lose_neighbor : node_t -> unit
  method is_neighbor : node_t -> bool
  method neighbors : Common.NodeSet.t

  method move : Coord.coordf_t -> unit
  method recv_pkt : pkt:Packet.packet_t -> unit

  method dump_state : node_cnt:int -> node_state:node_state_t -> int

end


class node  ~pos_init ~id  : node_t = 

object(s)
  
  inherit Log.loggable

  val mutable neighbors  = Common.NodeSet.empty
  val mutable pos = pos_init
  val mutable bler_agent = None

  val id = id
  val mutable db = new NodeDB.nodeDB
  val agents = Hashtbl.create 1

   
  method pos = pos
  method id = id
  method x = Coord.x pos
  method y = Coord.y pos
  method private bler_agent = Misc.o2v bler_agent

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    objdescr <- (sprintf "/Node/%d" id);
    bler_agent <-  Some (new Bler_agent.bler_agent (s :> Node.node_t));
    (Gworld.world())#update_pos ~node:(s :> Node.node_t) ~oldpos_opt:None
  )

  method move newpos = (
    let oldpos = pos in
    pos <- newpos;
    (* important to call update_pos *after* our own position has been updated *)
    s#log_info (sprintf "Moved from %s to %s" (Coord.sprintf oldpos) (Coord.sprintf newpos));
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


  method dump_state ~node_cnt ~node_state = 1 + node_state.Node.node_pos;
(*    let b = {
    node_pos=s#pos;
    db_state=db#dump_state ~node_cnt:node_cnt
  } in b*)


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
