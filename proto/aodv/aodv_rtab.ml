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

open Aodv_defaults
open Aodv_pkt

type rtab_entry_t = {
  seqno: Aodv_pkt.seqno_t option; 
  (* seqno is either Some i, or None, which is equivalent to the 
     'Valid Destination Sequence Number flag' being false (RFC 6.2, 6.5, 6.7)
  *)
  nexthop: Common.nodeid_t;
  hopcount: int ;

  precursors: Common.nodeid_t list;
  (* Updated when fwd (RFC 6.7) or originate (RFC 6.6.2) a RREP.
     Used when a link breaks to know who to send rerr to (RFC 6.11). 
     set when fwd a rrep. *)

  lifetime: Time.time_t;
  (* Stored as absolute time.

     Read in from corresponding field in RREP packet (RFC 6.2)
     Updated for reverse-path when fwding a RREP packet (RFC 6.7)
     Updated each time we succesfully use the route. (RFC 6.2)
     Computed when we receive a RREQ  (RFC 6.5)

     Note that this plays dual role of expiry time and delete time (RFC 6.11),
     depending on the value of the 'valid' flag.
  *)
  
  valid:bool; (* invalidate when receive RERR, link break while
		 forwarding, or notice link break through gap in hello
		 message stream (RFC 6.11).
		 An invalid route is kept around for its hopcount (for
		 determining RREQ TTLs), .. ?
	      *)
}

type t = (Common.nodeid_t, (bool * rtab_entry_t option)) Hashtbl.t
  (* AODV routing tables are represented as a hashtbl, indexed by node id,
     with values being the couple (repairing, entry_opt) where entry_opt is
     None if we have no routing entry for the node *)

let repairing rt dst = 
  try fst (Hashtbl.find rt dst) with Not_found -> false


let create size = Hashtbl.create size

let have_entry rt dst = 
  try snd (Hashtbl.find rt dst) <> None with Not_found -> false

let get_entry_opt rt dst = 
  try snd (Hashtbl.find rt dst) with Not_found -> None


let replace_entry rt dst e = 
    Hashtbl.replace rt dst ((repairing rt dst), Some e)

let remove_entry (rt : t) dst :  unit = 
  if repairing rt dst then 
    Hashtbl.replace rt dst (true, None)
  else Hashtbl.remove rt dst

let repair_start rt dst = 
  Hashtbl.replace rt dst (true, get_entry_opt rt dst)
 
let repair_end rt dst = 
  Hashtbl.replace rt dst (false, get_entry_opt rt dst)

(* This checks if a route entry has become invalid (inactive) or, has
   expired. It should be called first thing by any function here which is
   going to return  routing state to the user. 
*)
let update_timeout_valid rt dst = 
  match get_entry_opt rt dst with
    | None -> ()
    | Some e -> let time = Time.time() in
      match e.valid, e.lifetime > time with
	  _, true -> ()
	| true, false -> 
	    if e.lifetime +. aodv_DELETE_PERIOD > time then 
	      replace_entry rt dst 
		{e with valid=false; lifetime=e.lifetime +. aodv_DELETE_PERIOD}
	    else remove_entry rt dst
	| false, false -> remove_entry rt dst 
	      
let seqno rt dst  = 
  update_timeout_valid rt dst;
  match get_entry_opt rt dst with
    | None -> None
    | Some e -> e.seqno


let incr_seqno rt dst =  
  match get_entry_opt rt dst with
    | None -> () 
    | Some e -> match e.seqno with
	| None -> ()
	| Some sn -> replace_entry rt dst { e with seqno=Some (sn + 1)}
	      
  
let set_seqno rt dst seqno =   
  match get_entry_opt rt dst with
    | None -> ()
    | Some e -> replace_entry rt dst {e with seqno=Some seqno}


let lifetime rt dst = 
  update_timeout_valid rt dst;
  match get_entry_opt rt dst with
    | None -> 0.0
    | Some e -> (assert (e.lifetime -. (Time.time()) > 0.0));
	e.lifetime -. (Time.time()) 

  
let set_lifetime rt dst time = 
  match get_entry_opt rt dst with
    | None -> ()
    | Some e -> replace_entry rt dst 
	{e with lifetime=Time.time() +. time}
	
	
let add_entry_neighbor rt dst = 
  begin match get_entry_opt rt dst with
    | None -> 
	replace_entry rt dst 
	{nexthop=dst; 
	hopcount=1;
	seqno=None; 
	lifetime=Time.time() +. aodv_ACTIVE_ROUTE_TIMEOUT; 
	valid=true;
	precursors=[]}
    | Some e ->
	replace_entry rt dst 
	{e with 
	  nexthop=dst; 
	  hopcount=1;
	  seqno=None; 
	  lifetime=Time.time() +. aodv_ACTIVE_ROUTE_TIMEOUT; 
	  valid=true}
  end;
  repair_end rt dst

let add_entry_rrep rt rrep sender = 
  update_timeout_valid rt rrep.rrep_dst;
  let update_entry = 
    match get_entry_opt rt rrep.rrep_dst with
      | None -> true
      | Some e -> 
	(* check cases (i), (ii), (iii), (iv) from rfc 6.7 to see if we should
	   update entry. *)
	(e.seqno = None) ||
	(rrep.rrep_dst_sn > Opt.get e.seqno) ||
	((rrep.rrep_dst_sn = Opt.get e.seqno) &&
	e.valid = false) ||
	((rrep.rrep_dst_sn = Opt.get e.seqno) &&
	rrep.rrep_hopcount + 1 < e.hopcount) in
  if update_entry then (
    replace_entry rt rrep.rrep_dst 
      {nexthop=sender; 
      hopcount=rrep.rrep_hopcount + 1;
      seqno=Some rrep.rrep_dst_sn; 
      lifetime=Time.time() +. aodv_ACTIVE_ROUTE_TIMEOUT; 
      valid=true;
      precursors=[]};
    repair_end rt rrep.rrep_dst);
  update_entry


let add_entry_rreq rt rreq sender = 
  update_timeout_valid rt rreq.rreq_orig;
  let new_seqno = 
    (match get_entry_opt rt rreq.rreq_orig with
      |	None -> rreq.rreq_orig_sn
      | Some e -> let old_seqno = seqno rt rreq.rreq_orig in
	if old_seqno = None then rreq.rreq_orig_sn 
	else max rreq.rreq_orig_sn (Opt.get old_seqno)) in
  let minimalLT = 
    2.0 *. aodv_NET_TRAVERSAL_TIME
    -. 2.0 *. (float rreq.rreq_hopcount) *. aodv_NODE_TRAVERSAL_TIME
  and currentLT = lifetime rt rreq.rreq_orig in
  let new_lifetime = Time.time() +. (max minimalLT currentLT) in
  assert (rreq.rreq_hopcount > 0);
  (* if this assert fails, then i need to look at how/when rreq hopcounts are
     being incremented *)
  replace_entry rt rreq.rreq_orig 
    {nexthop=sender; 
    hopcount=rreq.rreq_hopcount;
    seqno=Some new_seqno; 
    lifetime=new_lifetime;
    valid=true;
    precursors=[]
    }
    

let dests_thru_hop rt nexthop = 
  (** [dests_thru_hop rtab nexthop] returns the list (dest, seqno) pairs for
    all dests for which whom [rtab] contains a valid entry, with next hop
    equal to [nexthop]. *)
  let f dst (_, e) l = 
    match e with
	None -> l
      | Some e -> 
	  if e.valid && e.nexthop = nexthop then dst::l else l in
  Hashtbl.fold f rt []
    
let precursors rt dst = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> []
    | Some e -> e.precursors
  
let h = Hashtbl.create 20
let have_many_precursors rt nodelist = 
  Hashtbl.clear h;
  let b = ref false in 
  let f pre = if Hashtbl.mem h pre then b := true else Hashtbl.add h pre () in
  List.iter (fun node -> List.iter f (precursors rt node)) nodelist;
  !b
  


let has_precursors rt dst = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> false
    | Some e -> e.precursors <> []
  

let add_precursor rt  ~dst ~pre = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> raise Not_found
    | Some e -> replace_entry rt dst 
	{e with precursors=pre::e.precursors}

let nexthop rt dst = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> raise Not_found
    | Some e -> if e.valid then e.nexthop else raise Not_found



let nexthop_invalid rt dst = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> raise Not_found
    | Some e -> e.nexthop
  

let nexthop_opt rt dst = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> None
    | Some e -> if e.valid then Some e.nexthop else None

let hopcount rt dst = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> raise Not_found
    | Some e -> e.hopcount
  
let hopcount_opt rt dst = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> None
    | Some e -> Some e.hopcount
 
let invalidate rt dst =
  match get_entry_opt rt dst with
    | None -> ()
    | Some e -> replace_entry rt dst {e with valid=false}

let valid rt dst = 
  update_timeout_valid rt dst;  
  match get_entry_opt rt dst with
    | None -> false
    | Some e -> e.valid

let clear_entry rt dst = Hashtbl.remove rt dst
  
let clear_all_entries rt  = Hashtbl.clear rt


let have_active_route rt = 
  Hashtbl.fold (fun dst _ b -> b || valid rt dst) rt false
