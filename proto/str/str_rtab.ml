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


(* xxx question : in add_entry, should we alwasy add an entry with equal
   seqnos, or only when current entry is invalid? *)

open Str_defaults
open Str_pkt

let (>>>) (sn1, d1) (sn2, d2) = 
  if sn1 > sn2 then true 
  else if sn1 = sn2 && d1 < d2 then true 
  else false
let (===) (sn1, d1) (sn2, d2) = (sn1, d1) = (sn2, d2)
   


type rtab_entry_t = {
  seqno: Str_pkt.seqno_t; 
  timestamp:Time.t;
  nexthop: Common.nodeid_t;
  hopcount: int ;
  valid:bool; (* invalidate when receive RERR, link break while
		 forwarding, or notice link break through gap in hello
		 message stream (RFC 6.11).
		 An invalid route is kept around for its hopcount (for
		 determining RREQ TTLs), .. ?
	      *)
}

type t = (Common.nodeid_t, (bool * rtab_entry_t list)) Hashtbl.t
  (* STR routing tables are represented as a hashtbl, indexed by node id,
     with values being the couple (repairing, entry_opt) where entry_opt is
     None if we have no routing entry for the node *)

let repairing rt dst = 
  try fst (Hashtbl.find rt dst) with Not_found -> false


let create size = Hashtbl.create size

let have_entry rt dst = 
  try snd (Hashtbl.find rt dst) <> [] with Not_found -> false

let get_entry_opt rt dst = 
  try snd (Hashtbl.find rt dst) with Not_found -> []


let replace_entry rt dst e = 
    Hashtbl.replace rt dst ((repairing rt dst), [e])

let remove_entry (rt : t) dst :  unit = 
  if repairing rt dst then 
    Hashtbl.replace rt dst (true, [])
  else Hashtbl.remove rt dst

let repair_start rt dst = 
  Hashtbl.replace rt dst (true, get_entry_opt rt dst)
 
let repair_end rt dst = 
  Hashtbl.replace rt dst (false, get_entry_opt rt dst)

      
let seqno rt dst  = 
  match get_entry_opt rt dst with
    | [] -> None
    | [e] -> Some e.seqno
    | _ -> failwith "xxx not dealing with multiple entries yet"

let set_seqno rt dst seqno =   
  match get_entry_opt rt dst with
    | [] -> ()
    | [e] -> replace_entry rt dst {e with seqno=seqno}
    | _ -> failwith "xxx not dealing with multiple entries yet"


let add_entry rt ~dst ~seqno ~nh ~hc = 
  assert (hc > 0);
  (* If this assert fails, then i need to look at how/when rreq hopcounts are
     being incremented *)
    begin match get_entry_opt rt dst with
      | [] -> replace_entry rt dst
	  {seqno=seqno;
	  timestamp=Time.time();
	  nexthop=nh;
	  hopcount=hc;
	  valid=true};
	  repair_end rt dst;
	  true
      | [e] -> if (seqno, hc) >>> (e.seqno, e.hopcount) ||
	  ((not e.valid) && (seqno, hc) === (e.seqno, e.hopcount)) 
	then (
	  replace_entry rt dst
	  {seqno=seqno;
	  timestamp=Time.time();
	  nexthop=nh;
	  hopcount=hc;
	  valid=true};
	  repair_end rt dst;
	  true
	) else false
      | _ -> failwith "xxx not dealing with multiple entries yet"
    end    


let dests_thru_hop rt nexthop = 
  (** [dests_thru_hop rtab nexthop] returns the list (dest, seqno) pairs for
    all dests for which whom [rtab] contains a valid entry, with next hop
    equal to [nexthop]. *)
  let f dst (_, e) l = 
    match e with
      | [] -> l
      | [e] -> if e.valid && e.nexthop = nexthop then dst::l else l 
      | _ -> failwith "xxx not dealing with multiple entries yet" in
  Hashtbl.fold f rt []
    
let nexthop rt dst = 
  match get_entry_opt rt dst with
    | [] -> raise Not_found
    | [e] -> if e.valid then e.nexthop else raise Not_found
    | _ -> failwith "xxx not dealing with multiple entries yet"

let nexthop_invalid rt dst = 
  match get_entry_opt rt dst with
    | [] -> raise Not_found
    | [e] -> e.nexthop
    | _ -> failwith "xxx not dealing with multiple entries yet"

let nexthop_maybe rt dst = 
  match get_entry_opt rt dst with
    | [] -> None
    | [e] -> if e.valid then Some e.nexthop else None
    | _ -> failwith "xxx not dealing with multiple entries yet"

let hopcount rt dst = 
  match get_entry_opt rt dst with
    | [] -> raise Not_found
    | [e] -> e.hopcount
    | _ -> failwith "xxx not dealing with multiple entries yet"
  
let hopcount_maybe rt dst = 
  match get_entry_opt rt dst with
    | [] -> None
    | [e] -> Some e.hopcount
    | _ -> failwith "xxx not dealing with multiple entries yet"
 
let invalidate rt dst =
  match get_entry_opt rt dst with
    | [] -> ()
    | [e] -> replace_entry rt dst {e with valid=false}
    | _ -> failwith "xxx not dealing with multiple entries yet"

let valid rt dst = 
  match get_entry_opt rt dst with
    | [] -> false
    | [e] -> e.valid
    | _ -> failwith "xxx not dealing with multiple entries yet"

let clear_entry rt dst = Hashtbl.remove rt dst
  
let clear_all_entries rt  = Hashtbl.clear rt


let have_active_route rt = 
  Hashtbl.fold (fun dst _ b -> b || valid rt dst) rt false
