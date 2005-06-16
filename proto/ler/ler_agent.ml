(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)

(* TODO: Improve georouting so that it looks for 
   the *furthest* node within range that is closer to destination, and 
   if no such node exists, then it looks for the closest node 
   outside range that is closer to destination. This will avoid making
   zillions of tiny hops.*)

type ler_proto_t = EASE | GREASE | FRESH

open Printf
let sp = Printf.sprintf

let ntargets = 
  Param.intcreate 
    ~name:"Number of LER targets" 
    ~default:1
    ~doc:"Number of targets in Simulation"
    ()


type persist_t =  
    {le_state : Le_tab.le_tab_state_t;
    proto:ler_proto_t}

let agents_array_ = 
  Array.init Node.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let agents ?(stack=0) () = agents_array_.(stack)
let agent ?(stack=0) i = 
  Hashtbl.find agents_array_.(stack) i





class ler_agent ?(stack=0) ~proto theowner = 
object(s)
  
  (* We inherit from the base routing agent class. This is documented in
     rt_agent_base.ml and rt_agent.mli. *)
  inherit [unit, persist_t] Rt_agent_base.base_persist ~stack theowner 
    
  val mutable le_tab = new Le_tab.le_tab ~ntargets:(Param.get ntargets)

  val mutable fresh = if proto = FRESH then true else false 
  val mutable grease = if proto = GREASE then true else false (* EASE or GREASE? *)
    
  method le_tab = le_tab


  initializer (
    let agent = match proto with 
      | GREASE -> "/grease_agent" 
      | EASE -> "/ease_agent" 
      | FRESH -> "/fresh_agent" in

    s#set_objdescr ~owner:(theowner :> Log.inheritable_loggable) agent;

    Hashtbl.replace agents_array_.(stack) theowner#id (s :> ler_agent);

    (* Here we ask the global world object to inform us each time a node enters
       our neighborhood, by calling our method add_neighbor.
       The global world object (and its method add_new_ngbr_hook) are documented
       in worldt.ml 
       Of course in a real protocol this is not possible: this would be done
       with periodic hello messages.
    *)
    (World.gw())#add_new_ngbr_hook theowner#id ~hook:s#add_neighbor;
  )

  (* This is called each time a node enters our neighborhood. 
     We insert an entry in our encounter table.*)
  method private add_neighbor nid = (
    if nid < (Param.get ntargets) then (
      let n = Nodes.node nid in
      le_tab#add_encounter ~nid ~pos:n#pos;
    )
  )

  (* This method is called each time a packet is received at the node. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = 
    s#recv_ler_pkt_ l3pkt 


  (* [app_recv_l4pkt] is the entry point from upper (L4) layers which have a 
     packet to send. We build the L3 header and originate the packet into the
     EASE routing logic. *)
  method recv_pkt_app l4pkt dst = (

    if dst <> L3pkt.l3_bcast_addr then
      let ler_hdr = Ler_pkt.make_ler_hdr 
	~anchor_pos:owner#pos ~enc_age:(le_tab#le_age dst)  in
      let l3hdr = 
	L3pkt.make_l3hdr ~src:myid ~dst:dst ~ext:(`LER_HDR ler_hdr) () in
      let l3pkt =  L3pkt.make_l3pkt ~l3hdr ~l4pkt in
      s#recv_ler_pkt_ l3pkt;
    else
      Log.log#log_info (lazy "Dropping packet with bcast destination..");
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
		((World.w())#dist_coords (Nodes.node n)#pos anchor_pos) >
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
      let anchor = Opt.get (le_tab#le_pos dst) in

      (* Return triplet as defined above *)
      (0.0, anchor, our_enc_age, owner#id) 

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
	    (agent ~stack nid)#le_tab#le_age dst < cur_enc_age)
	  ()
	)
      in
      if (msngr = dst) then 
	(((World.w())#dist_coords owner#pos (Nodes.node dst)#pos), 
	(Nodes.node dst)#pos, 
	0.0, dst)
      else
	let enc_age = (agent ~stack msngr)#le_tab#le_age dst
	and enc_pos = Opt.get ((agent ~stack msngr)#le_tab#le_pos dst)
	in
	let d_to_messenger = 
	  (World.w())#dist_coords owner#pos (Nodes.node msngr)#pos 
	in
	
	(* Return triplet as defined above *)
	(d_to_messenger, enc_pos, enc_age, msngr) 
    )
  )


  method private we_are_closest_to_anchor anchor_pos = 
    (s#closest_toward_anchor anchor_pos) = myid;



  method private fw_pkt_ pkt = (
    let dst = (L3pkt.l3dst pkt) in

    (* If the destination is within range, bypass georouting and send directly
       to it. *)
    if (World.w())#are_neighbors owner#id dst then 
      s#mac_send_pkt pkt dst
    else 
      s#geo_fw_pkt_ pkt
  )

  (* Geographically forward a packet to the next hop. The georouting algorithm
     is described in ler_agent.mli *)
  method private geo_fw_pkt_ pkt = (
    let dst = (L3pkt.l3dst pkt) in

    (* this first case is necssary to avoid a possible infinite loop if we are at
       the same position as the destination, in which case the find_closest call
       in closest_toward_anchor might return us. *)
    if owner#pos = (Nodes.node dst)#pos  then 

      (* [mac_send_pkt] is documented in node.mli. *)
      s#mac_send_pkt pkt (Nodes.node dst)#id

    else (
      (* find next closest node toward anchor *)
      let ler_hdr = L3pkt.ler_hdr pkt in
      let closest_id = s#closest_toward_anchor (Ler_pkt.anchor ler_hdr) in
      
      if closest_id = myid then (
	
	s#log_debug (lazy (sp "We are closest to anchor"));
	s#log_debug (lazy (sp "our_pos: %s, dst_pos:%s" (Coord.sprintf owner#pos)
	  (Coord.sprintf (Nodes.node dst)#pos)));
	
	s#recv_ler_pkt_ pkt
      ) else (   
	(* geographically forward toward anchor  *)
	s#log_debug (lazy (sp "Forwarding geographically to %d" closest_id));
	s#log_debug (lazy (sp "our_pos: %s, dst_pos:%s" (Coord.sprintf owner#pos)
	  (Coord.sprintf (Nodes.node dst)#pos)))
      );

      (* [mac_send_pkt] is documented in node.mli. *)
      s#mac_send_pkt pkt closest_id
    )
  )
    

  (* The core logic implementing EASE. *)
  method private recv_ler_pkt_ pkt = (

    let dst = L3pkt.l3dst pkt in
    let ler_hdr = L3pkt.ler_hdr pkt in

    s#log_info 
      (lazy (sp "received pkt with src %d, dst %d, enc_age %f, anchor_pos %s"
	(L3pkt.l3src pkt)
	(L3pkt.l3dst pkt)
 	(Ler_pkt.enc_age ler_hdr)
	(Coord.sprintf (Ler_pkt.anchor ler_hdr))
      ));
    
    Ler_pkt.set_search_dist ler_hdr 0.0;

    match myid = dst with
	
      | true -> (* We are destination. *)
	  s#log_debug (lazy (sp "packet has arrived"));

      | false ->  (* We are src or intermediate hop *)

	  let cur_enc_age = (Ler_pkt.enc_age ler_hdr) in
	  if (
	    s#we_are_closest_to_anchor (Ler_pkt.anchor ler_hdr) || 
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

	    let (d_to_msnger, next_anchor, next_enc_age, msngr) = 
	      s#find_next_anchor dst cur_enc_age
	    in
	    Ler_pkt.set_enc_age ler_hdr next_enc_age;

	    Ler_pkt.set_search_dist ler_hdr d_to_msnger;

	    if fresh then 
	      Ler_pkt.set_anchor_pos ler_hdr ((World.w())#nodepos msngr)
	    else
	      Ler_pkt.set_anchor_pos ler_hdr next_anchor;

	  );

	  (* Forward packet toward anchor.*)
	  s#fw_pkt_ pkt
  )
    
  method stats = ()
  method reset_stats() = ()
    
  method dump_state () = 
    {le_state = le_tab#dump_state;
    proto=proto;
    }

  method read_state (s : persist_t) = 
    let new_tab = new Le_tab.le_tab ~ntargets:(Param.get ntargets) in
    new_tab#load_state s.le_state;
    le_tab <- new_tab

end

module Persist : Persist.t = 
struct
  type description = int array
      (* array indexed by stack #, contains the number of agents to be read
	 for each stack. *)
      
  let save oc = 
    Log.log#log_notice (lazy "Saving LER agent states..");

    let descr : description = 
      Array.init Node.max_nstacks 
	(fun stack -> Misc.hashlen agents_array_.(stack))
    in
    Marshal.to_channel oc descr [];

    for stack = 0 to Node.max_nstacks - 1 do
      let agents = agents_array_.(stack) in
      Hashtbl.iter (fun nid agent -> 
	Marshal.to_channel oc (nid, agent#dump_state()) []) agents
    done;
    Log.log#log_notice (lazy "Done.")


  let restore ic = 

    Log.log#log_notice (lazy (sp "Restoring LER agent states..."));
    let descr = (Marshal.from_channel ic : description) in

    Array.iteri (fun stack n_agents -> 
      for i = 0 to n_agents - 1 do
	let (nid, state) = 
	  (Marshal.from_channel ic : Common.nodeid_t * persist_t) in
	let node = Nodes.node nid in
	let agent = 
	  (new ler_agent ~stack ~proto:state.proto node) in 
	agent#read_state state;
	node#install_rt_agent ~stack ( agent :> Rt_agent.t);
      done;
    ) descr;

    Log.log#log_notice (lazy "Done. (restoring LER agent states)")

end
