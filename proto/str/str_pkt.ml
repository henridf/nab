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



open Pkt_common

type age_t = float
let sprint_age age = Printf.sprintf "age: %f, sn: %d" age

let unknown_age = -.1.
let age_size = 4 
let hopcount_size = 1
let rreq_id_size = 2

type data_hdr = {
  data_src_age : age_t;
  data_hopcount : int; 
  data_dst_age : float; (* these two fields should be updated by each node *)
  data_dst_hc : int     (* forwarding the data packet *)
}

type rreq_hdr = {
  rreq_orig :     Common.nodeid_t;
  rreq_orig_age : age_t;
  rreq_hopcount : int; 
  rreq_id :       int;
  rreq_dst :      Common.nodeid_t; 
  rreq_dst_age :  age_t;       
  rreq_dst_hc :   int;          
}

type rrep_hdr = {
  rrep_replier : Common.nodeid_t; (* node which sent the RREP. this is only
				     really used for logging purposes. *)
  rrep_hopcount : int;        (* distance traversed by this RREP *)
  rrep_dst : Common.nodeid_t; (* dest to which this rrep offers a route. *)
  rrep_dst_age : age_t;       (* age and hopcount of *)
  rrep_dst_hc : int;    (* replying node's entry to destination *)
  rrep_orig : Common.nodeid_t;(* originator of rreq which elicited this rrep *)
}

type t = 
  | HELLO
  | DATA of data_hdr
  | RREQ of rreq_hdr
  | RREP of rrep_hdr

let hdr_size str_hdr = 
  match str_hdr with
    | HELLO -> 0
    | DATA _ -> age_size + hopcount_size
    | RREQ _ -> (2 * addr_size) + (2 * age_size) + (2 * hopcount_size) +
	rreq_id_size 
    | RREP _ -> (2 * addr_size) + hopcount_size + age_size


let clone str_pkt = str_pkt


let make_data_hdr ~src_age ~src_hc ~dst_age ~dst_hc =
  DATA 
    {data_src_age=src_age; 
    data_hopcount=src_hc; 
    data_dst_age=dst_age;
    data_dst_hc=dst_hc}
    

let make_rreq_hdr
  ~orig ~orig_age ~hc ~rreq_dst ~rreq_dst_age ~rreq_id ~rreq_dst_hc = 

  RREQ {
    rreq_orig=orig;
    rreq_orig_age=orig_age;
    rreq_id=rreq_id;
    rreq_hopcount=hc;
    rreq_dst=rreq_dst;
    rreq_dst_age=rreq_dst_age;
    rreq_dst_hc=rreq_dst_hc
  }

let make_rrep_hdr ~replier ~hc ~dst ~dst_hc ~dst_age ~orig = 
  RREP {
    rrep_replier=replier;
    rrep_hopcount=hc;
    rrep_dst=dst;
    rrep_dst_age=dst_age;
    rrep_dst_hc=dst_hc;
    rrep_orig=orig
  }


