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
    inherit Rt_agent.t
    inherit Log.inheritable_loggable

    val mutable db : NodeDB.nodeDB
    val owner : Gpsnode.gpsnode
    method add_neighbor : Common.nodeid_t -> unit
    method db : NodeDB.nodeDB
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





class ease_agent ?(stack=0) theowner = 
object(s)

  inherit Log.inheritable_loggable
  inherit Rt_agent_base.base ~stack theowner 

  val mutable db = new NodeDB.nodeDB (Param.get Params.ntargets)

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    s#set_objdescr  "/Ease_Agent";
    (World.gw())#add_new_ngbr_hook theowner#id ~hook:s#add_neighbor
  )

   method add_neighbor nid = (
     if nid < (Param.get Params.ntargets) then (
       let n = Nodes.gpsnode nid in
       db#add_encounter ~nid ~enc:(Common.enc ~time:(Common.get_time()) ~place:n#pos);
     )
   )

  method mac_recv_l3pkt l3pkt = 
    s#recv_ease_pkt_ l3pkt 

  method mac_recv_l2pkt _ = ()

  method private our_enc_age dst = 
      match db#last_encounter ~nid:dst with
	| None -> max_float
	| Some enc ->  Common.enc_age enc

  method app_recv_l4pkt (l4pkt:L4pkt.t) dst = (
    let ease_hdr = 
      Ease_pkt.make_ease_hdr
	~anchor_pos:owner#pos
	~enc_age:(s#our_enc_age dst)
    in	
    let l3hdr = 
      L3pkt.make_l3hdr 
	~srcid:myid 
	~dstid:dst 
	~ext:(`EASE_HDR ease_hdr)
	() 
    in
    let l3pkt =
      L3pkt.make_l3pkt ~l3hdr ~l4pkt:`NONE
    in
    s#recv_ease_pkt_ l3pkt;
  )

  method private closest_toward_anchor anchor_pos = (

      match (anchor_pos = owner#pos) with
	| true -> 
	    myid; (* we are the anchor (probably we are the src) *)
	| false ->
	    
	    let d_here_to_anchor = (World.w())#dist_coords owner#pos anchor_pos in
	    
	    let f nid = 
	      if (World.w())#dist_coords 
		((World.w())#nodepos nid) anchor_pos < d_here_to_anchor then true 
	      else false
	    in
	    match ((World.w())#find_closest ~pos:owner#pos ~f)
	    with 
	      | None -> myid
	      | Some n when (
		  ((World.w())#dist_coords (Nodes.gpsnode n)#pos anchor_pos) >
		  d_here_to_anchor)
		  ->
		  myid
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
	  (World.w())#find_closest 
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
	(((World.w())#dist_coords owner#pos (Nodes.gpsnode dst)#pos), 
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
	  (World.w())#dist_coords owner#pos (Nodes.gpsnode msngr)#pos in
	(d_to_messenger, enc.Common.p, Common.enc_age enc)
    )
  )


  method private we_are_closest_to_anchor anchor_pos = 
    (s#closest_toward_anchor anchor_pos) = myid;


  method private geo_fw_pkt_ pkt = (
    let dst = (L3pkt.l3dst pkt) in

    (* this first case is necssary to avoid a possible infinite loop if we are at
       the same position as the destination, in which case the find_closest call
       in closest_toward_anchor might return us *)
    if owner#pos = (Nodes.gpsnode (L3pkt.l3dst pkt))#pos  then 
      s#cheat_send_pkt pkt (Nodes.gpsnode dst)#id
    else (
      (* find next closest node toward anchor *)
      let ease_hdr = L3pkt.ease_hdr pkt in
      let closest_id = s#closest_toward_anchor (Ease_pkt.anchor ease_hdr) in
      
      if closest_id = myid then (
	
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
      s#cheat_send_pkt pkt closest_id
    )
  )
    
  method private recv_ease_pkt_ pkt = (

    let l3hdr = L3pkt.l3hdr pkt in
    let ease_hdr = L3pkt.ease_hdr pkt in

    s#log_info 
    (lazy (sprintf "%d received pkt with src %d, dst %d, enc_age %f, anchor_pos %s"
      myid 
      (L3pkt.l3src pkt)
      (L3pkt.l3dst pkt)
      (Ease_pkt.enc_age ease_hdr)
      (Coord.sprintf (Ease_pkt.anchor ease_hdr))
    ));
    
    Ease_pkt.set_search_dist ease_hdr 0.0;

    match  myid = (L3pkt.l3dst pkt) with
	
      | true -> (* We are destination. *)
	  s#log_debug (lazy (sprintf "packet has arrived"));
      | false -> (  
	  let cur_enc_age = (Ease_pkt.enc_age ease_hdr) in
	  if (
	    (* comment out the first condition to have ease instead of grease *)
	    (s#have_better_anchor  (L3pkt.l3dst pkt) cur_enc_age) ||
	    (s#we_are_closest_to_anchor (Ease_pkt.anchor ease_hdr))) then (
	    (* we are closest node to anchor. next anchor search  *)
	    (* We are src or intermediate hop *)
	    s#log_debug (lazy (sprintf "We are first or intermediate hop"));
	    let (d_to_msnger, next_anchor, next_enc_age) = 
	      s#find_next_anchor (L3pkt.l3dst pkt) cur_enc_age
	    in
	    Ease_pkt.set_enc_age ease_hdr next_enc_age;
	    Ease_pkt.set_anchor_pos ease_hdr next_anchor;
	    Ease_pkt.set_search_dist ease_hdr d_to_msnger;
	  );
	  (* Send through our containing nodes' mac layer *)
	  s#geo_fw_pkt_ pkt
	)
  )
    
    
    


end
