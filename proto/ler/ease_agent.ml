(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)







open Printf



let agents_array_ = [||]
let set_agents ?(stack=0) arr = agents_array_.(stack) <- arr
let agents ?(stack=0) () = agents_array_.(stack)
let agent ?(stack=0) i = agents_array_.(stack).(i)


let proportion_met_nodes ?(stack=0) () = 
  let targets = Param.get Params.ntargets in
  let total_encounters = 
    Array.fold_left (fun encs agent -> (agent#le_tab#num_encounters) + encs) 0
      (agents ~stack ())
  in
  (float total_encounters) /. (float ((Param.get Params.nodes) * targets))





class ease_agent ?(stack=0) ~grease theowner = 
object(s)
  
  (* We inherit logging methods from Log.inheritable_loggable.
     Most classes in mws inherit from it. *)
  inherit Log.inheritable_loggable

  (* We inherit from the base routing agent class. This is documented in
     rt_agent_base.ml and rt_agent.mli. *)
  inherit Rt_agent_base.base ~stack theowner 
    
  val mutable le_tab = new Le_tab.le_tab (Param.get Params.ntargets)

  val grease = grease (* EASE or GREASE? *)
    
  method le_tab = le_tab
  method set_le_tab tab = le_tab <- tab

  initializer (
    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable) "/Ease_Agent";

    agents_array_.(stack).(theowner#id) <- (s :> ease_agent);

    (* Here we ask the global world object to inform us each time a node enters
       our neighborhood, by calling our method add_neighbor.
       The global world object (and its method add_new_ngbr_hook) are documented
       in worldt.ml *)
    (World.gw())#add_new_ngbr_hook theowner#id ~hook:s#add_neighbor;
  )

  (* This is called each time a node enters our neighborhood. 
     We insert an entry in our encounter table.*)
  method private add_neighbor nid = (
    if nid < (Param.get Params.ntargets) then (
      let n = Nodes.gpsnode nid in
      le_tab#add_encounter ~nid ~pos:n#pos;
    )
  )

  (* This method is called each time a packet is received at the node. *)
  method mac_recv_l3pkt l3pkt = 
    s#recv_ease_pkt_ l3pkt 

  (* This method is called each time a packet is received at the node. It
     provides us with the full L2 header, which we don't care for, so this is
     a null method. *)
  method mac_recv_l2pkt _ = ()

  (* [app_recv_l4pkt] is the entry point from upper (L4) layers which have a 
     packet to send. We build the L3 header and originate the packet into the
     EASE logic. *)
  method app_recv_l4pkt (l4pkt:L4pkt.t) dst = (
    let ease_hdr = 
      Ease_pkt.make_ease_hdr
	~anchor_pos:owner#pos
	~enc_age:(le_tab#le_age dst)
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


  (* [closest_toward_anchor pos] returns the node_id of the closest node to us
     which is closer to pos than we are. *)
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
	  match ((World.w())#find_closest ~pos:owner#pos ~f ())
	  with 
	    | None -> myid
	    | Some n when (
		((World.w())#dist_coords (Nodes.gpsnode n)#pos anchor_pos) >
		d_here_to_anchor)
		->
		myid
	    | Some n -> n
  )

  (* [have_better_anchor dst age] returns [true] if our last encounter with
     [dst] is fresher than [age], false otherwise. *)
  method private have_better_anchor dst cur_enc_age = 
    (le_tab#le_age dst) < cur_enc_age


  (* [find_next_anchor d age] finds the next anchor for destination [d],
     assuming that the current best encounter age is [age]. 

     It returns a triplet (d, anch, age) consisting of:
     - d : distance to the msngr node which gave us this anchor (0 if found
     in our own table)
     - anch : (x,y) coord of the anchor point.
     - age : encounter age of this anchor. *)
  method private find_next_anchor dst cur_enc_age = (
    (* when did we see dst ? *)
    let our_enc_age = le_tab#le_age dst in

    if our_enc_age < cur_enc_age then (
      s#log_debug (lazy "Need new anchor, found one locally");
      let anchor = Opt.get (le_tab#le_pos ~nid:dst) in

      (* Return triplet as defined above *)
      (0.0, anchor, our_enc_age) 

    ) else (
      s#log_debug (lazy "Need new anchor, looking remotely");
      (* who's seen dst more recently than pkt.l3hdr.enc_age ? *)
      let msngr =  
	Opt.get (
	  (World.w())#find_closest 
	  (* the inequality has to be sharp to ensure that we make. But if
	     we are right next to the destination, it could be that our last
	     encounter was 'now', in which case the destination won't
	     satisfy the inequality, hence the first test *)
	  ~pos:owner#pos 
	  ~f:(fun nid -> 
	    (nid = dst)
	    ||
	    (agent ~stack nid)#le_tab#le_age ~nid:dst < cur_enc_age)
	  ()
	)
      in
      if (msngr = dst) then 
	(((World.w())#dist_coords owner#pos (Nodes.gpsnode dst)#pos), 
	(Nodes.gpsnode dst)#pos, 
	0.0)
      else
	let enc_age = (agent ~stack msngr)#le_tab#le_age ~nid:dst
	and enc_pos = Opt.get ((agent ~stack msngr)#le_tab#le_pos ~nid:dst)
	in
	let d_to_messenger = 
	  (World.w())#dist_coords owner#pos (Nodes.gpsnode msngr)#pos 
	in
	
	(* Return triplet as defined above *)
	(d_to_messenger, enc_pos, enc_age) 

    )
  )


  method private we_are_closest_to_anchor anchor_pos = 
    (s#closest_toward_anchor anchor_pos) = myid;


  (* Geographically forward a packet to the next hop. The georouting algorithm
     is described in ease_agent.mli *)
  method private geo_fw_pkt_ pkt = (
    let dst = (L3pkt.l3dst pkt) in

    (* this first case is necssary to avoid a possible infinite loop if we are at
       the same position as the destination, in which case the find_closest call
       in closest_toward_anchor might return us. *)
    if owner#pos = (Nodes.gpsnode (L3pkt.l3dst pkt))#pos  then 

      (* [cheat_send_pkt] is documented in simplenode.mli. *)
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

      (* [cheat_send_pkt] is documented in simplenode.mli. *)
      s#cheat_send_pkt pkt closest_id
    )
  )
    

  (* The core logic implementing EASE. *)
  method private recv_ease_pkt_ pkt = (

    let l3hdr = L3pkt.l3hdr pkt in
    let dst = L3pkt.l3dst pkt in
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

    match myid = dst with
	
      | true -> (* We are destination. *)
	  s#log_debug (lazy (sprintf "packet has arrived"));

      | false ->  (* We are src or intermediate hop *)

	  let cur_enc_age = (Ease_pkt.enc_age ease_hdr) in
	  if (
	    s#we_are_closest_to_anchor (Ease_pkt.anchor ease_hdr) || 
	    grease && (* if false, short-circuit boolean evaluation means 
			 that we don't do the test below, and hence don't
			 consult our local encounter table. *)
	    s#have_better_anchor dst cur_enc_age
	  ) then (
	    (* If we enter this branch, then either 
	       a) we are have arrived at the anchor, or 
	       b) we have ourselves a better anchor (and grease is turned on). 
	       Either way, we call #find_next_anchor which will either
	       return the better anchor from our own table (if case b above)
	       or it will find the closest neighboring node with a better
	       anchor (if case a above). *)

	    let (d_to_msnger, next_anchor, next_enc_age) = 
	      s#find_next_anchor dst cur_enc_age
	    in
	    Ease_pkt.set_enc_age ease_hdr next_enc_age;
	    Ease_pkt.set_anchor_pos ease_hdr next_anchor;
	    Ease_pkt.set_search_dist ease_hdr d_to_msnger;
	  );

	  (* Send through our containing nodes' mac layer *)
	  s#geo_fw_pkt_ pkt

  )
    
    
    


end

