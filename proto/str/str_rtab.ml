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
open Str_pkt
module H = ExtHashtbl.Hashtbl

type rtab_entry_t = {
  timestamp:Time.t; (* time at which this entry is added into our rtab *)
  last_used:Time.t; (* last time this route was used (or value of timestamp
		       above if never used). *)
  age:Str_pkt.age_t; (* age of the entry at the time it was added *)
  nexthop: Common.nodeid_t;
  hopcount: int ;
  valid:bool; (* invalidate when receive RERR, link break while
		 forwarding, or notice link break through gap in hello
		 message stream (RFC 6.11).
		 An invalid route is kept around for its hopcount (for
		 determining RREQ TTLs), .. ?
	      *)
}


type entryset = (int, rtab_entry_t) H.t
type t = (Common.nodeid_t, (bool * entryset)) H.t
    (* STR routing tables are represented as a hashtbl, indexed by node id,
       with values being the couple (repairing, entrylist) where entrylist is
       a hash of rtab_entry_t entries, with key=hopcount, value=entry. *)

    
let max_diameter = 50.
let max_age = 10000.

let binding_metric_ age hops = 
  let speed = Mob_ctl.get_speed_mps 0
  and range = (Param.get Params.radiorange) in
  (* AODV  *)
  age *. max_diameter +. (float hops)

  (* FRESH
  if age <= 1.0 then (float hops)
  else 
  (float hops) *. max_age +. age
*)

  
(*
  let range = (Param.get Params.radiorange) in
  let hops_cost hops =  (sqrt (float hops)) *. range in
  if hops = 0 then 0.0 
  else 
  let speed = Mob_ctl.get_speed_mps 0
  in (hops_cost hops) +. speed *. age
*)

let cost (a, d) = binding_metric_ a d

let age_ entry = Time.time() -. entry.timestamp +. entry.age

let (><) entry1 entry2 = 
  (age_ entry1) <= (age_ entry2) && entry1.hopcount <= entry2.hopcount
  (* [true] if entry1 excludes entry2 *)

let cost_ ?(offset=0) entry = 
  let offset_cost = 
    if offset >= 0 then
      binding_metric_ 0.0 offset
    else 0.0 in
  offset_cost  +.
    binding_metric_ (age_ entry) (entry.hopcount + offset)
    
let new_entryset_ () = H.create 8

let repairing rt dst = 
  try fst (H.find rt dst) with Not_found -> false

let create size = H.create size

let get_entries_ rt dst = 
  try snd (H.find rt dst) with Not_found -> new_entryset_()

let get_best_entry_opt_ rt ?(offset=0) ?(cost=max_float) dst =
  let best_so_far = ref (-1, cost) in
  let f i entry = 
    if cost_ ~offset entry < snd !best_so_far then 
      best_so_far := i, cost_ entry in
  try 
    let entries = snd (H.find rt dst) in
      H.iter f entries;
    if fst !best_so_far <> -1 then
      Some (H.find entries (fst !best_so_far))
    else None
  with Not_found -> None

let valid_ entry = 
  entry.valid && entry.last_used +. str_ACTIVE_ROUTE_TIMEOUT > Time.time()

let get_best_valid_entry_index_opt_ rt ?(cost=max_float) dst = 
  let best_so_far = ref (-1, cost) in
  let f i entry = 
    if valid_ entry && cost_ entry < snd !best_so_far then 
      best_so_far := i, cost_ entry in
  try 
    let entries = snd (H.find rt dst) in
    H.iter f entries;
    if fst !best_so_far <> -1 then
      Some (fst !best_so_far, H.find entries (fst !best_so_far))
    else None
  with Not_found -> None

let get_best_valid_entry_opt_ rt ?(cost=max_float) dst = 
  match get_best_valid_entry_index_opt_ rt ~cost dst with
    | None -> None
    | Some (index, entries) -> Some entries
	

let replace_entry_ rt dst e = 
    H.replace rt dst ((repairing rt dst), [e])

let repair_start rt dst = 
  H.replace rt dst (true, get_entries_ rt dst)
 
let repair_end rt dst = 
  H.replace rt dst (false, get_entries_ rt dst)
      
let sprint_entry_ i e = 
  (Printf.sprintf 
    "Entry %d: Age=%.2f[s], hopcount=%d, cost:%f, valid=%b.\n"
    i (age_ e) e.hopcount (cost_ e) (valid_ e))

let sprint_entries_ entries = 
  let str = Buffer.create 64 in
  let f i entry = Buffer.add_string str 
    (sprint_entry_ i entry) in
  H.iter f entries;
  Buffer.contents str

let sprint_best_entry rt dst = 
  match get_best_entry_opt_ rt dst with 
    | None -> "No entry for dst"
    | Some e -> sprint_entry_ (-1) e

let sprint_best_valid_entry rt dst = 
  match get_best_valid_entry_opt_ rt dst with 
    | None -> "No valid entry for dst"
    | Some e -> sprint_entry_ (-1) e

let sprint_entries rt dst = 
  let str = Buffer.create 64 in
  Buffer.add_string str (Printf.sprintf "Entries for destination %d: \n" dst);
  begin try 
    let entries = snd (H.find rt dst) in
    Buffer.add_string str (sprint_entries_ entries);
  with Not_found -> () end;
  Buffer.contents str

(* return true if 
   a) no entries lie in the exclusion area of another entry
   b) max one entry per hopcount
   c) Hopcount in entry is same as hash key
 *)
let check_ok_ entries = 
  let check_a, check_b, check_c = ref true, ref true, ref true in

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

!check_a && !check_b && !check_c
    
    
    
let add_entry rt ~dst ~age ~nh ~hc = 
  (* entries are kept in order of increasing distance *)
  let new_entry = 
    {timestamp=Time.time();
    last_used=Time.time();
    age=age;
    nexthop=nh;
    hopcount=hc;
    valid=true} in
  let entries = get_entries_ rt dst in
  match H.length entries with 
    | 0 -> 
	let new_entries = new_entryset_() in
	H.add new_entries hc new_entry;
	H.add rt dst (false, new_entries);
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
	  assert (
	    if H.fold (fun hc entry same_distance -> 
	      same_distance || hc = new_entry.hopcount)
	      entries false then (
		Log.log#log_error (lazy (sprint_entries rt dst));
		false) else true
	  );
	  assert (not (H.mem entries hc));
	  (* insert new_entry *)
	  H.add entries hc new_entry;
	  assert (check_ok_ entries);
	  true
	) else (
	  assert (check_ok_ entries);
	  false;
	)

let nexthop rt dst = 
  match get_best_valid_entry_opt_ rt dst with
    | None -> raise Not_found
    | Some e -> e.nexthop 

let nexthop_opt rt dst = 
  match get_best_valid_entry_opt_ rt dst with
    | None -> None
    | Some e -> Some e.nexthop 

let invalidate_nexthop rt nexthop =
  let entries_iter entries i entry = 
    if valid_ entry && entry.nexthop = nexthop then 
      H.replace entries i {entry with valid=false}
  in
  let hash_iter dst (_, e) =  H.iter (entries_iter e) e in
  H.iter hash_iter rt

let using_entry rt dst = 
  match get_best_valid_entry_index_opt_ rt dst with
    | None -> failwith "Str_rtab.using_entry"
    | Some (index, entry) -> 
	let entries = snd (H.find rt dst) in
	H.replace entries index {entry with last_used = Time.time()}

let age_dist rt dst = 
  match get_best_entry_opt_ rt dst with
    | None -> raise Not_found
    | Some e -> (age_ e), e.hopcount

let valid_age_dist rt dst = 
  match get_best_valid_entry_opt_ rt dst with
    | None -> raise Not_found
    | Some e -> (age_ e), e.hopcount

let hopcount rt dst = 
  match get_best_entry_opt_ rt dst with
    | None -> raise Not_found
    | Some e -> e.hopcount
	
let hopcount_opt rt dst = 
  Opt.map (fun e -> e.hopcount) (get_best_entry_opt_ rt dst)
 
let have_better_valid_route rt cost dst = 
  match get_best_valid_entry_opt_ rt ~cost dst with
    | None -> false
    | Some e -> true

let have_better_route rt ?(offset=0) cost dst = 
  match get_best_entry_opt_ rt ~offset ~cost dst with
    | None -> false
    | Some e -> true


let clear_entry rt dst = H.remove rt dst
  
let clear_all_entries rt  = H.clear rt


