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

let proportion_met_nodes ~targets  = 
  let total_encounters = 
    Array.fold_left (fun encs agent -> (agent#db#num_encounters) + encs) 0 !agents_array
  in
  (float total_encounters) /. (float ((Param.get Params.nodes) * targets))





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


   method add_neighbor n = (
     if n#id < ntargets then (
       db#add_encounter ~nid:n#id ~enc:(Common.enc ~time:(Common.get_time()) ~place:n#pos);
     )
   )

  method mac_recv_hook l3pkt = 
    s#recv_ease_pkt_ l3pkt 

  method private our_enc_age dst = 
      match db#last_encounter ~nid:dst with
	| None -> max_float
	| Some enc ->  Common.enc_age enc

  method app_send (l4pld:Packet.l4pld_t) ~dst = (
    s#recv_ease_pkt_ 
    (Packet.make_ease_l3pkt 
      ~srcid:owner#id 
      ~dstid:dst 
      ~anchor_pos:owner#pos
      ~enc_age:(s#our_enc_age dst)
    )
  )

  method private closest_toward_anchor anchor_pos = (

      match (anchor_pos = owner#pos) with
	| true -> 
	    owner#id; (* we are the anchor (probably we are the src) *)
	| false ->
	    
	    let d_here_to_anchor = (Gworld.world())#dist_coords owner#pos anchor_pos in
	    
	    let f node = 
	      if (Gworld.world())#dist_coords node#pos anchor_pos < d_here_to_anchor then true 
	      else false
	    in
	    match ((Gworld.world())#find_closest ~pos:owner#pos ~f)
	    with 
	      | None -> owner#id
	      | Some n when (
		  ((Gworld.world())#dist_coords (Nodes.node n)#pos anchor_pos) >
		  d_here_to_anchor)
		  ->
		  owner#id
	      | Some n -> n
  )

  method private have_better_anchor dst cur_enc_age = 
    (s#our_enc_age dst) < cur_enc_age
    
  method private find_next_anchor dst cur_enc_age = (
    (* when did we see dst ? *)
    let our_enc_age = s#our_enc_age dst in
    

    if our_enc_age < cur_enc_age then (
      s#log_debug "Need new anchor, found one locally\n";
      let anchor = (Misc.o2v (db#last_encounter ~nid:dst)).Common.p in
      (0.0, anchor, our_enc_age)
    ) else (
      s#log_debug "Need new anchor, looking remotely\n";
      (* who's seen dst more recently than pkt.l3hdr.enc_age ? *)
      let msngr =  
	Misc.o2v (
	  (Gworld.world())#find_closest 
	  (* the inequality has to be sharp to ensure that we make. But if
	     we are right next to the destination, it could be that our last
	     encounter was 'now', in which case the destination won't
	     satisfy the inequality, hence the first test *)
	  owner#pos 
	  (fun n -> 
	    (n#id = dst)
	    ||
	    !agents_array.(n#id)#db#encounter_age ~nid:dst < cur_enc_age)
	)
      in
      if (msngr = dst) then 
	(((Gworld.world())#dist_coords owner#pos (Nodes.node dst)#pos), 
	(Nodes.node dst)#pos, 
	0.0) 
      else
	let enc = 
	  Misc.o2v (
	    !agents_array.(msngr)#db#last_encounter
	    ~nid:dst 
	  )
	in
	let d_to_messenger = 
	  (Gworld.world())#dist_coords owner#pos (Nodes.node msngr)#pos in
	(d_to_messenger, enc.Common.p, Common.enc_age enc)
    )
  )



  method private we_are_closest_to_anchor anchor_pos = 
    (s#closest_toward_anchor anchor_pos) = owner#id;


  method private geo_fw_pkt_ pkt = (
    (* this first case is necssary to avoid a possible infinite loop if we are at
       the same position as the destination, in which case the find_closest call
       in closest_toward_anchor might return us *)
    
    if owner#pos = (Nodes.node pkt.l3hdr.dst)#pos  then 
      owner#cheat_send_pkt ~l3pkt:pkt ~dstid:(Nodes.node pkt.l3hdr.dst)#id
    else (
      (* find next closest node toward anchor *)
      let closest_id = s#closest_toward_anchor pkt.l3hdr.anchor_pos in
      
      if closest_id = owner#id then (
(*
	s#log_debug (sprintf "We are closest to %d" closest_id);
	s#log_debug (sprintf "our_pos: %s, dst_pos:%s" (Coord.sprintf owner#pos)
	  (Coord.sprintf (Nodes.node pkt.l3hdr.dst)#pos));
*)
	s#recv_ease_pkt_ pkt
      ) else (   
	(* geographically forward toward anchor  *)
(*      	s#log_debug (sprintf "Forwarding geographically to %d" closest_id);
	s#log_debug (sprintf "our_pos: %s, dst_pos:%s" (Coord.sprintf owner#pos)
	  (Coord.sprintf (Nodes.node pkt.l3hdr.dst)#pos));
	
*)      );
      owner#cheat_send_pkt ~l3pkt:pkt ~dstid:closest_id;
      
    )
  )


  method private recv_ease_pkt_ pkt = (
    s#log_info 
    (sprintf "%d received pkt with src %d, dst %d, enc_age %f, anchor_pos %s"
      owner#id 
      pkt.l3hdr.src 
      pkt.l3hdr.dst
      pkt.l3hdr.ease_enc_age
      (Coord.sprintf pkt.l3hdr.anchor_pos)
    );
    
    pkt.l3hdr.search_dist <- 0.0;
    match  owner#id = pkt.l3hdr.dst with
	
      | true -> (* We are destination. *)
	  s#log_debug (sprintf "packet has arrived");
      | false -> (  
	  let cur_enc_age = pkt.l3hdr.ease_enc_age in
	  if (
	    (* comment out the first condition to have ease instead of grease *)
	    (s#have_better_anchor  pkt.l3hdr.dst cur_enc_age) ||
	    (s#we_are_closest_to_anchor pkt.l3hdr.anchor_pos)) then (
	    (* we are closest node to anchor. next anchor search  *)
	    (* We are src or intermediate hop *)
	    (*	    s#log_debug (sprintf "We are first or intermediate hop");*)
	    
	    let (d_to_msnger, next_anchor, next_enc_age) = 
	      s#find_next_anchor pkt.l3hdr.dst cur_enc_age
	    in
	    pkt.l3hdr.ease_enc_age <- next_enc_age;
	    pkt.l3hdr.anchor_pos <- next_anchor;
	    (* Send through our containing nodes' mac layer *)
	    pkt.l3hdr.search_dist <- d_to_msnger;
	  );
	  s#geo_fw_pkt_ pkt
	)
  )
    
    
    


end
