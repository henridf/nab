(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Packet
open Printf

(* ease_agent: 
   Simplified ease algorithm which simply sends packets from anchor 
   to anchor. Does not do a real flooding.
*)


class type ease_agent_t = 
  object
    val mutable db : NodeDB.nodeDB
    val ntargets : Common.nodeid_t
    val mutable objdescr : string
    val owner : Node.node_t
    method add_neighbor : Node.node_t -> unit
    method app_send : Packet.l4pld_t -> dst:Common.nodeid_t -> unit
    method db : NodeDB.nodeDB
    method mac_recv_hook : Packet.l3packet_t -> unit
    method objdescr : string
    method set_db : NodeDB.nodeDB -> unit
    method private recv_ease_pkt_ : Packet.l3packet_t -> unit
  end

let agents_array = ref ([||]: ease_agent_t array)

let set_agents arr = agents_array := arr
let agent i = !agents_array.(i)

(* right now this mhook does not distinguish betwen src-dst pairs, it takes
   anything and tries to construct the route.
   Later, it should take src and dst as parameters, and script would create a
   closured mhook with appropriate src and dsts.
   This would allow having multiple route constructions ongoing and mhooks
   demuxing between them apropriately.
*)

(*
let magic_bler_route_mhook routeref l2pkt (node:Node.node_t) = (

  let l3dst = (Packet.get_l3hdr l2pkt.l3pkt).dst 
  and l3src = (Packet.get_l3hdr l2pkt.l3pkt).src in
  
  match l2pkt.l2hdr.l2src <> node#id with
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
(*	    Route.anchor_age=0.0;*)
	    Route.anchor_age=(node#db)#encounter_age ~nid:l3dst;
	    Route.searchcost=0.0 (* will be filled in when the packet
				    leaves this intermediate node below *)
	  }
  
    | false ->  (* Packet leaving some node *)

	if  node#id = l3src then  (* Packet leaving src *)
	  	    
	  routeref := Route.add_hop !routeref {
	    Route.hop=node#id;
	    Route.anchor=node#id;
	    Route.anchor_age=(node#db)#encounter_age ~nid:l3dst;
	    Route.searchcost=0.0
	  }

	else    (* Packet leaving intermediate node. *)

	  (* now we know the next hop, we can figure out the search cost *)
	  let next_hop = 
	    match l2pkt.l2hdr.l2dst with
	      | L2_DST d -> d
	      | L2_BCAST -> 
		  raise (Failure "Bler_agent.bler_route_mhook: didn't expect to see a L2_BCAST")
	  in
	  (Route.last_hop !routeref).Route.searchcost <- 
	  (((Gworld.world())#dist_nodeids next_hop node#id) ** 2.0)
)
*)


class ease_agent owner = 
object(s)

  inherit Log.loggable

  val owner:Node.node_t = owner


  val mutable db = new NodeDB.nodeDB (Param.get Params.ntargets)
  val ntargets = Param.get Params.ntargets

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    objdescr <- (owner#objdescr ^  "/Ease_Agent");
    owner#add_recv_pkt_hook ~hook:s#mac_recv_hook;
    owner#add_app_send_pkt_hook ~hook:s#app_send;
    owner#add_new_ngbr_hook ~hook:s#add_neighbor
  )

  method mac_recv_hook l3pkt = ()
(*    s#recv_ease_pkt_ ~pkt:l3pkt *)

   method add_neighbor n = (
     if n#id < ntargets then (
       db#add_encounter ~nid:n#id ~enc:(Common.enc ~time:(Common.get_time()) ~place:n#pos);
     )
   )


  method app_send (l4pld:Packet.l4pld_t) ~dst = (
    let enc_age = (
      match db#last_encounter ~nid:dst with
	| None -> max_float
	| Some enc ->  Common.enc_age enc
    ) in

    s#recv_ease_pkt_ 
      (Packet.make_ease_l3pkt 
	~srcid:owner#id 
	~dstid:dst 
	~anchor:owner#pos
	~enc_age
      )

  )


  method private closest_toward_anchor anchor_pos = (

      match (anchor_pos = owner#pos) with
	| true -> owner#id (* we are the anchor (probably we are the src) *)
	| false ->
	    
	    let d_here_to_anchor = (Gworld.world())#dist_coords owner#pos anchor_pos in
	    
	    let f node = 
	      if (Gworld.world())#dist_coords node#pos anchor_pos < d_here_to_anchor then true 
	      else if 
		(Gworld.world())#dist_coords node#pos owner#pos > d_here_to_anchor  
	      then raise Misc.Break
	      else false
	    in
	    
	    let closestopt = 
		begin
		  try 
		    (Gworld.world())#find_closest ~pos:owner#pos ~f
		  with 
		    | Misc.Break -> Some owner#id 
		    | o -> raise o
		end	    
	    in 
	    if closestopt = None then raise (Misc.Impossible_Case
	      "Ease_agent.recv_ease_pkt_");
	    Misc.o2v closestopt;
	    (*	    if closest = None then raise (Misc.Impossible_Case "Ease_agent.recv_ease_pkt_");
		    Misc.o2v closest*)
  )
  method private recv_ease_pkt_ pkt = (
    s#log_info (sprintf "%d received pkt with src %d, dst %d"
      owner#id (pkt.l3hdr.src) (pkt.l3hdr.dst));

    (* find next closest node toward anchor *)
    let closest_id = s#closest_toward_anchor pkt.l3hdr.anchor_pos in
    
    if closest_id != owner#id then
      (* geographically forward toward anchor  *)
      owner#cheat_send_pkt ~l3pkt:pkt ~dstid:closest_id
    else (  (* we are closest node to anchor. next anchor search  *)

      match  owner#id = pkt.l3hdr.dst with
	  
	| true -> (* We are destination. *)
	    s#log_debug (sprintf "packet has arrived");

	| false ->  (* We are src or intermediate hop *)

	    s#log_debug (sprintf "We are first or intermediate hop");
	    
	    (* when did we see dst ? *)
	    let our_enc_age = (
	      match db#last_encounter ~nid:pkt.l3hdr.dst with
		| None -> max_float
		| Some enc ->  Common.enc_age enc
	    ) 
	    in
	    let (next_anchor, next_enc_age) = 
	      if our_enc_age < pkt.l3hdr.ease_enc_age then
		let anchor = (Misc.o2v (db#last_encounter ~nid:pkt.l3hdr.dst)).Common.p in
		(anchor, our_enc_age)
	      else
		(* who's seen dst more recently than pkt.l3hdr.enc_age ? *)
		let closest_with_better_anchor =  
		  Misc.o2v (
		    (Gworld.world())#find_closest 
		    (* the inequality has to be sharp to ensure that we make. But if
		       we are right next to the destination, it could be that our last
		       encounter was 'now', in which case the destination won't
		       satisfy the inequality, hence the first test *)
		    owner#pos 
		    (fun n -> 
		      n#id = pkt.l3hdr.dst  ||
		      !agents_array.(n#id)#db#encounter_age ~nid:pkt.l3hdr.dst < our_enc_age)
		  )
		in
		let enc = Misc.o2v (
		  !agents_array.(closest_with_better_anchor)#db#last_encounter
		  ~nid:pkt.l3hdr.dst 
		)
		in
		(enc.Common.p, enc.Common.t)

		in
	    pkt.l3hdr.ease_enc_age <- next_enc_age;
	    pkt.l3hdr.anchor_pos <- next_anchor;
	    (* Send through our containing nodes' mac layer *)
	    s#recv_ease_pkt_ pkt
    )
  )


end
