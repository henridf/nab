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


open Str_defaults
module Str = Str_pkt


module H = ExtHashtbl.Hashtbl

type rtab_entry_t = {
  timestamp:Time.t; (* time at which this entry is added into our rtab *)
  last_used:Time.t; (* last time this route was used (or value of timestamp
		       above if never used). *)
  age:Str.age_t; (* age of the entry at the time it was added *)
  seqno:Str.seqno_t; (* seqno of the entry. *)
  nexthop: Common.nodeid_t;
  hopcount: int ;
  valid:bool; (* invalidate when receive RERR, link break while
		 forwarding, or notice link break through gap in hello
		 message stream (RFC 6.11).
		 An invalid route is kept around for its hopcount (for
		 determining RREQ TTLs), .. ?
	      *)
}

type metric_t = AODV | STR


type entryset = (int, rtab_entry_t) H.t
type t = (Common.nodeid_t, entryset) H.t
    (* STR routing tables are represented as a hashtbl, indexed by node id,
       with values entrylist where entrylist is
       a hash of rtab_entry_t entries, with key=hopcount, value=entry. *)

let max_diameter = 50.
let max_age = 10000.

let (>>>) ent1 ent2 = 
  if ent1 <> Str_pkt.null_triple && ent2 = Str_pkt.null_triple then true
  else
    ent1.Str.sn > ent2.Str.sn || 
    (ent1.Str.sn = ent2.Str.sn && ent1.Str.hc < ent2.Str.hc) 

let aodv_metric age hops = max_float
(*
  if age < 3.0 then 
    age *. max_diameter +. (float hops)
  else max_float
  (* AODV without timeouts *)
     age *. max_diameter +. (float hops)
*)
let str_metric age hops = 
  let speed = Mob_ctl.get_speed_mps 0
  and range = (Param.get Params.radiorange) in
  (* STR *)
  let range = (Param.get Params.radiorange) in
  let hops_cost hops =  (float hops) *. (range *. 0.7) in
  if hops = 0 then 0.0 
  else 
  let speed = Mob_ctl.get_speed_mps 0
  in (hops_cost hops) +. 2. *. speed *. age

let binding_metric_ metric age hops = 
  match metric with 
    | AODV -> aodv_metric age hops
    | STR -> str_metric age hops

  (*

  
  (* FRESH *)
  if age <= 1.0 then (float hops)
  else 
    (float hops) *. max_age +. age
  *)

  



let age_ entry = Time.time() -. entry.timestamp +. entry.age

let (><) entry1 entry2 = 
  (age_ entry1) <= (age_ entry2) && entry1.hopcount <= entry2.hopcount
  (* [true] if entry1 excludes entry2 *)

let cost_ ?(offset=0) metric entry = 
  let offset_cost = 
    if offset >= 0 then
      binding_metric_ metric 0.0 offset
    else 0.0 in
  offset_cost  +.
    binding_metric_ metric (age_ entry) entry.hopcount 
    
let cost metric ent = 
  binding_metric_ metric ent.Str.age ent.Str.hc


let new_entryset_ () = H.create 8

let create size = H.create size

let get_entries_ rt dst = 
  try H.find rt dst with Not_found -> new_entryset_()

let best_invalid_entry_ rt metric ?(usable=false) ?(offset=0) ?(cost=max_float) dst =
  let best_so_far = ref (-1, cost) in
  let f i entry = 
    if cost_ metric ~offset entry < snd !best_so_far &&
      not usable || entry.last_used +. str_ACTIVE_ROUTE_TIMEOUT > Time.time()
    then 
      best_so_far := i, cost_ metric entry in
  try 
    let entries = H.find rt dst in
      H.iter f entries;
    if fst !best_so_far <> -1 then
      Some (H.find entries (fst !best_so_far))
    else None
  with Not_found -> None

let valid_ entry = 
  entry.valid && entry.last_used +. str_ACTIVE_ROUTE_TIMEOUT > Time.time()

let triple_of_entry_ e = {Str.hc=e.hopcount; Str.sn=e.seqno; Str.age=(age_ e)}

let best_valid_entry_ rt ?(cost=max_float) dst = 
  let best_so_far = ref Str.null_triple in
  let f i entry = 
    if valid_ entry && 
      {Str.age=0.0; Str.hc=entry.hopcount; Str.sn=entry.seqno} 
      (* don't care about age in (>>>) *)
      >>>
      !best_so_far 
    then 
      best_so_far := triple_of_entry_ entry in
  try 
    let entries = H.find rt dst in
    H.iter f entries;
    if !best_so_far <> Str_pkt.null_triple then
      Some (H.find entries !best_so_far.Str.hc)
    else None
  with Not_found -> None
    
let best_valid_entry rt dst = 
  match best_valid_entry_ rt dst with
    | Some e -> triple_of_entry_ e, e.nexthop
    | None -> Str_pkt.null_triple, (-1)

let best_usable_invalid_entry rt metric dst = 
  match best_invalid_entry_  rt metric ~usable:true dst with
    | Some e -> triple_of_entry_ e, e.nexthop
    | None -> Str_pkt.null_triple, (-1)


let sprint_entry_ i e = 
  (Printf.sprintf 
    "Entry %d: age=%.2f[s], hc=%d, sn=%d cost=, valid=%b.\n"
    i (age_ e) e.hopcount e.seqno (*(cost_ e)*) (valid_ e))

let sprint_entries_ entries = 
  let str = Buffer.create 64 in
  let f i entry = Buffer.add_string str 
    (sprint_entry_ i entry) in
  H.iter f entries;
  Buffer.contents str

let sprint_entries rt dst = 
  let str = Buffer.create 64 in
  Buffer.add_string str (Printf.sprintf "Entries for destination %d: \n" dst);
  begin try 
    let entries = H.find rt dst in
    Buffer.add_string str (sprint_entries_ entries);
  with Not_found -> () end;
  Buffer.contents str

(* return true if 
   a) no entries lie in the exclusion area of another entry
   b) max one entry per hopcount
   c) Hopcount in entry is same as hash key
   d) no invalid fields
 *)
let check_ok_ entries = 
  let check_a, check_b, check_c, check_d = 
    ref true, ref true, ref true, ref true in

  H.iter (fun h_i e_i ->
    H.iter (fun h_j e_j ->
	if h_i <> h_j then 
	  if e_i >< e_j || e_j >< e_i 
	  then check_a := false
    ) entries
  ) entries;
  if not !check_a then Log.log#log_error (lazy 
    (Printf.sprintf "check_a failed. Entries:\n%s" (sprint_entries_ entries)));
  
  H.iter (fun h e -> 
    if H.find_all entries h <> [H.find entries h] then (
      Log.log#log_error (lazy 
	(Printf.sprintf "check_b failed. Duplicates for hc %d" h));
      
      check_b := false
    )
  ) entries;

  H.iter (fun h e -> 
    if e.hopcount <> h then (
      Log.log#log_error (lazy 
	(Printf.sprintf "check_c failed. key: %d, hc %d" h e.hopcount));
      check_c := false)
    ) entries;

  H.iter (fun h e -> 
    if e.hopcount < 0 || e.hopcount > str_NET_DIAMETER then (
      Log.log#log_error (lazy 
	(Printf.sprintf "check_d failed. key: %d, hc %d" h e.hopcount));
      check_d := false);
    if e.seqno < 0 && e.valid then (
      Log.log#log_error (lazy 
	(Printf.sprintf "check_d failed. key: %d, seqno %d" h e.seqno));
      check_d := false);
    if e.age = max_float then (
      Log.log#log_error (lazy 
	(Printf.sprintf "check_d failed. key: %d, age %f" h e.age));
      check_d := false);    
  ) entries;
  

  !check_a && !check_b && !check_c && !check_d
    
    
    
let add_entry rt ~valid ~dst ~ent ~nh  = 
  (* need to take into account valid flag, and also check if entries =
     Str.null_st *)

  if ent = Str.null_triple then false else 
    
    (* entries are kept in order of increasing distance *)
    let new_entry = 
      {timestamp=Time.time();
      last_used=Time.time();
      age=ent.Str.age;
      seqno=ent.Str.sn;
      hopcount=ent.Str.hc;
      nexthop=nh;
      valid=valid} in
    let entries = get_entries_ rt dst in
    match H.length entries with 
      | 0 -> 
	  let new_entries = new_entryset_() in
	  H.add new_entries ent.Str.hc new_entry;
	  H.add rt dst new_entries;
	  assert (check_ok_ entries);
	  true
      | n -> 
	  (* Step_1. Iterate through existing entries:
	     - If any existing entry is excluded by new entry, remove them
	     - If new entry is excluded by any existing entry, then stop and
	     return false.
	     2. add new entry. *)
	  H.iter (fun hc entry -> 
	    if new_entry >< entry then 
	      H.remove entries hc
	  ) entries;
	  
	  let new_entry_excluded = 
	    H.fold (fun hc entry excluded -> excluded || entry >< new_entry)
	      entries false in
	  
	  if not new_entry_excluded then (

	    assert (not (H.mem entries ent.Str.hc));
	    (* insert new_entry *)
	    H.add entries ent.Str.hc new_entry;
	    assert (check_ok_ entries);
	    true
	  ) else (
	    assert (check_ok_ entries);
	    false;
	  )

let nexthop rt dst = 
  match best_valid_entry_ rt dst with
    | None -> raise Not_found
    | Some e -> e.nexthop 

let nexthop_opt rt dst = 
  match best_valid_entry_ rt dst with
    | None -> None
    | Some e -> Some e.nexthop 

let invalidate_nexthop rt nexthop =
  let entries_iter entries i entry = 
    if valid_ entry && entry.nexthop = nexthop then 
      H.replace entries i {entry with valid=false}
  in
  let hash_iter dst e =  H.iter (entries_iter e) e in
  H.iter hash_iter rt

let using_entry rt dst ent = 
  let entries = get_entries_ rt dst in
  let found = ref false in
  let f hc entry = 
    if entry.hopcount = ent.Str.hc && 
      age_ entry = ent.Str.age &&
    entry.seqno = ent.Str.sn
    then (
      assert (!found=false);
      H.replace entries hc {entry with last_used = Time.time()};
      found := true
    ) in
  H.iter f entries;
  if not !found then failwith "Str_rtab.using_entry"
	

let better_invalid_route rt metric ?(offset=false) hdr dst = 
  let ent = hdr.Str.i_ent in
  let offset = if offset then hdr.Str.orig_hc else 0 in
  match best_invalid_entry_ rt metric dst with
    | Some entry  -> 
	if ent = Str_pkt.null_triple ||
	  cost_ ~offset metric entry <  binding_metric_ metric ent.Str.age ent.Str.hc then
	  triple_of_entry_ entry
	else
	  Str_pkt.null_triple
    | None -> Str_pkt.null_triple

let better_usable_invalid_route rt metric ?(offset=false) hdr dst = 
  let ent = hdr.Str.i_ent in
  let offset = if offset then hdr.Str.orig_hc else 0 in
  match best_invalid_entry_ rt metric ~usable:true dst with
    | Some entry  -> 
	if ent = Str_pkt.null_triple ||
	  cost_ ~offset metric entry <  binding_metric_ metric ent.Str.age ent.Str.hc then
	    triple_of_entry_ entry, entry.nexthop
	else
	  Str_pkt.null_triple, (-1)
    | None -> Str_pkt.null_triple, (-1)

let better_valid_route rt ?(offset=false) hdr dst = 
  let ent = hdr.Str.v_ent in
  let offset = if offset then hdr.Str.orig_hc else 0 in
  match best_valid_entry_ rt dst with
    | Some entry -> 
	if ent = Str_pkt.null_triple || 
	  entry.seqno > ent.Str.sn ||
	  (entry.seqno = ent.Str.sn && 
	    entry.hopcount + offset < ent.Str.hc) then
	    triple_of_entry_ entry, entry.nexthop
	else
	  Str_pkt.null_triple, (-1)
    | None -> Str_pkt.null_triple, (-1)
	

let clear_entry rt dst = H.remove rt dst
  
let clear_all_entries rt  = H.clear rt


let is_usable rt ent dst = 
  let entries = H.find rt dst in 
  let entry = H.find entries ent.Str.hc in
  entry.last_used +. str_ACTIVE_ROUTE_TIMEOUT > Time.time()
