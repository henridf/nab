(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


open Printf

(* Magic_bler_agent: 
   Simplified bler algorithm which simply sends packets from anchor 
   to anchor. Does not do a real flooding.
   Is also incomplete in that it does not need to properly fill in 
   bler packets with the current encounter age. *)

(* right now this mhook does not distinguish betwen src-dst pairs, it takes
   anything and tries to construct the route.
   Later, it should take src and dst as parameters, and script would create a
   closured mhook with appropriate src and dsts.
   This would allow having multiple route constructions ongoing and mhooks
   demuxing between them apropriately.
*)

let magic_bler_route_mhook routeref l2pkt (node:Simplenode.simplenode) = (

  let l3pkt = (L2pkt.l3pkt l2pkt) in
  let l3dst = Packet.l3dst l3pkt in
  let l3src = Packet.l3src l3pkt in
  
  match (L2pkt.l2src l2pkt) <> node#id with
    | true -> 	(* Packet arriving at a node *)

	if  node#id = l3dst then  (* Packet arriving at dst. *)
	  	    
	  routeref := Route.add_hop !routeref {
	    Route.hop=node#id;
	    Route.anchor=node#id;
	    Route.anchor_age=0.0;
	    Route.searchcost=0.0
	  }

	else  (* Packet arriving at intermediate node *)
	  routeref := Route.add_hop !routeref {
	    Route.hop=node#id;
	    Route.anchor=node#id;
	    Route.anchor_age=0.0;
(*	    Route.anchor_age=(node#db)#encounter_age ~nid:l3dst;*)
	    Route.searchcost=0.0 (* will be filled in when the packet
				    leaves this intermediate node below *)
	  }
  
    | false ->  (* Packet leaving some node *)

	if  node#id = l3src then  (* Packet leaving src *)
	  	    
	  routeref := Route.add_hop !routeref {
	    Route.hop=node#id;
	    Route.anchor=node#id;
	    Route.anchor_age=0.0;
(*	    Route.anchor_age=(node#db)#encounter_age ~nid:l3dst;*)
	    Route.searchcost=0.0
	  }

	else    (* Packet leaving intermediate node. *)

	  (* now we know the next hop, we can figure out the search cost *)
	  let next_hop = 
	    match (L2pkt.l2dst l2pkt) with
	      | L2pkt.L2_DST d -> d
	      | L2pkt.L2_BCAST -> 
		  raise (Failure "Bler_agent.bler_route_mhook: didn't expect to see a L2pkt.L2_BCAST")
	  in
	  (Route.last_hop !routeref).Route.searchcost <- 
	  (((Gworld.world())#dist_nodeids next_hop node#id) ** 2.0)
)


class magic_bler_agent owner  = 
object(s)

  inherit Log.inheritable_loggable

  val owner:Simplenode.simplenode = owner

  val mutable db = new NodeDB.nodeDB (Param.get Params.ntargets)

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    objdescr <- (owner#objdescr ^  "/Bler_Agent");
    owner#add_recv_pkt_hook ~hook:s#mac_recv_hook;
    owner#add_app_send_pkt_hook ~hook:s#app_send
    (Gworld.world())#add_new_ngbr_hook owner#id ~hook:s#add_neighbor
  )

   method add_neighbor nid = (
     if nid < (Param.get Params.ntargets) then (
       let n = Nodes.gpsnode nid in
       db#add_encounter ~nid ~enc:(Common.enc ~time:(Common.get_time()) ~place:n#pos);
     )
   )

  method private our_enc_age dst = 
      match db#last_encounter ~nid:dst with
	| None -> max_float
	| Some enc ->  Common.enc_age enc

  method mac_recv_hook l3pkt = 
    match (Packet.l4pkt l3pkt) with
      | `BLER_PKT p -> s#recv_bler_pkt_ l3pkt
      | _ -> ()

  method app_send (l4pkt:L4pkt.t) ~dst = 
    s#recv_bler_pkt_ (Packet.make_bler_l3pkt ~srcid:owner#id ~dstid:dst)


  method private recv_bler_pkt_ (pkt:Packet.t) = (
    s#log_info (lazy (sprintf "%d received pkt with src %d, dst %d"
      owner#id (Packet.l3src pkt) (Packet.l3dst pkt)));

    match  owner#id = (Packet.l3dst pkt) with
	
      | true -> (* We are destination. *)
	  s#log_debug (lazy (sprintf "packet has arrived"));

      | false ->  (* We are src or intermediate hop *)

	  s#log_debug (lazy (sprintf "We are first or intermediate hop"));
	  
	  (* when did we see dst ? *)
	  
	  let our_encounter_age = (
	    
 	    match db#last_encounter ~nid:pkt.l3hdr.dst with
	      | None when (pkt.l3hdr.src <> owner#id) 
		  -> raise (Failure "Got bler packet for a node I have never seen\n")
	      | None ->  max_float (* we're src and we've never seen dst *) 
	      | Some enc ->  Common.enc_age enc
	  ) in

	  
	  (* who's seen dst more recently than us? *)
	  
	  let next_hop =  
	    (Gworld.world())#find_closest 
	    (* the inequality has to be sharp to ensure that we make. But if
	       we are right next to the destination, it could be that our last
	       encounter was 'now', in which case the destination won't
	       satisfy the inequality, hence the first test *)
	    owner#pos 
	      (fun n -> 
		n#id = pkt.l3hdr.dst ||
		(n#db)#encounter_age pkt.l3hdr.dst < our_encounter_age)

	  in
	  (* Send through our containing nodes' mac layer *)
	  owner#cheat_send_pkt ~l3pkt:pkt ~dstid:(Misc.o2v next_hop)
  )


end
