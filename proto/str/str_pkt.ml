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

type seqno_t = {dtime:Time.dtime_t; sn:int}
let sprint_seqno sn = Printf.sprintf "t: %d, sn: %d" sn.dtime sn.sn

let null_seqno = {dtime=0;sn=0}
let seqno_size = 4 + 2
let hopcount_size = 1
let rreq_id_size = 2
type common_hdr = {
  prev_hop_sn : seqno_t;
}
  
type data_hdr = {
  data_src_sn : seqno_t;
  data_hopcount : int; 
}

type rreq_hdr = {
  rreq_orig : Common.nodeid_t;
  rreq_orig_sn : seqno_t;
  rreq_hopcount : int; 
  rreq_id :          int;
  rreq_dst : Common.nodeid_t; (* Route request destination : RREQ *)
  rreq_dst_sn : seqno_t;  (* Destination Seqno: RREQ *)
  rreq_dst_hc : int;          (* Destination hopcount: RREQ *)
}

type rrep_hdr = {
  rrep_hopcount : int; 
  rrep_dst : Common.nodeid_t; (* dest to which this rrep offers a route. *)
  rrep_dst_sn : seqno_t;  
  rrep_orig : Common.nodeid_t;            (* originator of rreq which elicited this rrep *)
}

type proto_hdr = 
  | HELLO
  | DATA of data_hdr
  | RREQ of rreq_hdr
  | RREP of rrep_hdr

type t = {ch : common_hdr; ph : proto_hdr}

let common_hdr_size = seqno_size
let hdr_size str_hdr = 
  common_hdr_size + 
  match str_hdr.ph with
    | HELLO -> 0
    | DATA _ -> seqno_size + hopcount_size
    | RREQ _ -> (2 * addr_size) + (2 * seqno_size) + (2 * hopcount_size) +
	rreq_id_size 
    | RREP _ -> (2 * addr_size) + hopcount_size + seqno_size


let clone str_pkt = str_pkt


let make_data_hdr ~sn ~hc = DATA {data_src_sn=sn; data_hopcount=hc}

let make_rreq_hdr
  ~orig ~orig_sn ~hc ~rreq_dst ~rreq_dst_sn ~rreq_id ~rreq_dst_hc = 

  RREQ {
    rreq_orig=orig;
    rreq_orig_sn=orig_sn;
    rreq_id=rreq_id;
    rreq_hopcount=hc;
    rreq_dst=rreq_dst;
    rreq_dst_sn=rreq_dst_sn;
    rreq_dst_hc=rreq_dst_hc
  }

let make_rrep_hdr ~hc ~dst ~dst_sn ~orig = 
  RREP {
    rrep_hopcount=hc;
    rrep_dst=dst;
    rrep_dst_sn=dst_sn;
    rrep_orig=orig
  }


