(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf

(* ease_agent: 
   Simplified ease algorithm which simply sends packets from anchor 
   to anchor. Does not do a real flooding.
*)


class type ease_agent_t = 
  object
    val mutable db : NodeDB.nodeDB
    val owner : Gpsnode.gpsnode
    method add_neighbor : Common.nodeid_t -> unit
    method app_recv_l4pkt : L4pkt.t -> dst:Common.nodeid_t -> unit
    method db : NodeDB.nodeDB
    method mac_recv_l3pkt : L3pkt.t -> unit
    method objdescr : string
    method set_db : NodeDB.nodeDB -> unit
    method private recv_ease_pkt_ : L3pkt.t -> unit
  end

let agents_array = ref ([||]: ease_agent_t array)

let set_agents arr = agents_array := arr
let agent i = !agents_array.(i)

let proportion_met_nodes()   = 
  let targets = Param.get Params.ntargets in
  let total_encounters = 
    Array.fold_left (fun encs agent -> (agent#db#num_encounters) + encs) 0 !agents_array
  in
  (float total_encounters) /. (float ((Param.get Params.nodes) * targets))





class ease_agent owner = 
object(s)

  inherit Log.inheritable_loggable

  val owner:Gpsnode.gpsnode = owner


  val mutable db = new NodeDB.nodeDB (Param.get Params.ntargets)

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    s#set_objdescr  "/Ease_Agent";
    (Gworld.world())#add_new_ngbr_hook owner#id ~hook:s#add_neighbor
  )

   method add_neighbor nid = (
     if nid < (Param.get Params.ntargets) then (
       let n = Nodes.gpsnode nid in
       db#add_encounter ~nid ~enc:(Common.enc ~time:(Common.get_time()) ~place:n#pos);
     )
   )

  method mac_recv_l3pkt l3pkt = 
    s#recv_ease_pkt_ l3pkt 

  method private our_enc_age dst = 
      match db#last_encounter ~nid:dst with
	| None -> max_float
	| Some enc ->  Common.enc_age enc

  method app_recv_l4pkt (l4pkt:L4pkt.t) ~dst = (
    s#recv_ease_pkt_ 
    (L3pkt.make_ease_l3pkt 
      ~srcid:owner#id 
      ~dstid:dst 
      ~anchor_pos:owner#pos
      ~enc_age:(s#our_enc_age dst)
      ~l4pkt
    )
  )

  method private closest_toward_anchor anchor_pos = (

      match (anchor_pos = owner#pos) with
	| true -> 
	    owner#id; (* we are the anchor (probably we are the src) *)
	| false ->
	    
	    let d_here_to_anchor = (Gworld.world())#dist_coords owner#pos anchor_pos in
	    
	    let f nid = 
	      if (Gworld.world())#dist_coords 
		((Gworld.world())#nodepos nid) anchor_pos < d_here_to_anchor then true 
	      else false
	    in
	    match ((Gworld.world())#find_closest ~pos:owner#pos ~f)
	    with 
	      | None -> owner#id
	      | Some n when (
		  ((Gworld.world())#dist_coords (Nodes.gpsnode n)#pos anchor_pos) >
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
      s#log_debug (lazy "Need new anchor, found one locally\n");
      let anchor = (Misc.o2v (db#last_encounter ~nid:dst)).Common.p in
      (0.0, anchor, our_enc_age)
    ) else (
      s#log_debug (lazy "Need new anchor, looking remotely\n");
      (* who's seen dst more recently than pkt.l3hdr.enc_age ? *)
      let msngr =  
	Misc.o2v (
	  (Gworld.world())#find_closest 
	  (* the inequality has to be sharp to ensure that we make. But if
	     we are right next to the destination, it could be that our last
	     encounter was 'now', in which case the destination won't
	     satisfy the inequality, hence the first test *)
	  owner#pos 
	  (fun nid -> 
	    (nid = dst)
	    ||
	    !agents_array.(nid)#db#encounter_age ~nid:dst < cur_enc_age)
	)
      in
      if (msngr = dst) then 
	(((Gworld.world())#dist_coords owner#pos (Nodes.gpsnode dst)#pos), 
	(Nodes.gpsnode dst)#pos, 
	0.0)
      else
	let enc = 
	  Misc.o2v (
	    !agents_array.(msngr)#db#last_encounter
	    ~nid:dst 
	  )
	in
	let d_to_messenger = 
	  (Gworld.world())#dist_coords owner#pos (Nodes.gpsnode msngr)#pos in
	(d_to_messenger, enc.Common.p, Common.enc_age enc)
    )
  )


  method private we_are_closest_to_anchor anchor_pos = 
    (s#closest_toward_anchor anchor_pos) = owner#id;


  method private geo_fw_pkt_ pkt = (
    let dst = (L3pkt.l3dst pkt) in

    (* this first case is necssary to avoid a possible infinite loop if we are at
       the same position as the destination, in which case the find_closest call
       in closest_toward_anchor might return us *)
    if owner#pos = (Nodes.gpsnode (L3pkt.l3dst pkt))#pos  then 
      owner#cheat_send_pkt ~l3pkt:pkt (Nodes.gpsnode dst)#id
    else (
      (* find next closest node toward anchor *)
      let closest_id = s#closest_toward_anchor (L3pkt.l3anchor pkt) in
      
      if closest_id = owner#id then (
	
	s#log_debug (lazy (sprintf "We are closest to %d" closest_id));
	s#log_debug (lazy (sprintf "our_pos: %s, dst_pos:%s" (Coord.sprintf owner#pos)
	  (Coord.sprintf (Nodes.gpsnode dst)#pos)));
	
	s#recv_ease_pkt_ pkt
      ) else (   
	(* geographically forward toward anchor  *)
	s#log_debug (lazy (sprintf "Forwarding geographically to %d" closest_id));
	s#log_debug (lazy (sprintf "our_pos: %s, dst_pos:%s" (Coord.sprintf owner#pos)
	  (Coord.sprintf (Nodes.gpsnode dst)#pos)))
      );
      owner#cheat_send_pkt ~l3pkt:pkt closest_id
    )
  )
    
  method private recv_ease_pkt_ pkt = (
    let l3hdr = L3pkt.l3hdr pkt in

    s#log_info 
    (lazy (sprintf "%d received pkt with src %d, dst %d, enc_age %f, anchor_pos %s"
      owner#id 
      (L3pkt.l3src pkt)
      (L3pkt.l3dst pkt)
      (L3pkt.l3enc_age pkt)
      (Coord.sprintf (L3pkt.l3anchor pkt))
    ));
    
    L3pkt.set_search_dist ~l3hdr 0.0;

    match  owner#id = (L3pkt.l3dst pkt) with
	
      | true -> (* We are destination. *)
	  s#log_debug (lazy (sprintf "packet has arrived"));
      | false -> (  
	  let cur_enc_age = (L3pkt.l3enc_age pkt) in
	  if (
	    (* comment out the first condition to have ease instead of grease *)
	    (s#have_better_anchor  (L3pkt.l3dst pkt) cur_enc_age) ||
	    (s#we_are_closest_to_anchor (L3pkt.l3anchor pkt))) then (
	    (* we are closest node to anchor. next anchor search  *)
	    (* We are src or intermediate hop *)
	    s#log_debug (lazy (sprintf "We are first or intermediate hop"));
	    let (d_to_msnger, next_anchor, next_enc_age) = 
	      s#find_next_anchor (L3pkt.l3dst pkt) cur_enc_age
	    in
	    L3pkt.set_enc_age ~l3hdr next_enc_age;
	    L3pkt.set_anchor_pos ~l3hdr next_anchor;
	    L3pkt.set_search_dist ~l3hdr d_to_msnger;
	  );
	  (* Send through our containing nodes' mac layer *)
	  s#geo_fw_pkt_ pkt
	)
  )
    
    
    


end
