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
let age_size = 4 

let hopcount_size = 1

type seqno_t = int
let seqno_size = 4
type triple_t = {age:age_t; sn:seqno_t; hc:int}
let triple_size = 4 + 4 + 1

let null_triple = {age = max_float; sn = (-1); hc=max_int}




let rreq_id_size = 2

type str_hdr = {
  orig_hc : int; (* hopcount travelled since leaving originator *)
  orig_sn : seqno_t; (* sn of originator *)
  v_ent : triple_t; (* valid seqno, distance *)
  i_ent : triple_t; (* invalid age, distance *)
}

let str_hdr_size = 2 * triple_size + hopcount_size



type rreq_hdr = {
  rreq_orig :     Common.nodeid_t;
  rreq_id :       int;
  rreq_dst :      Common.nodeid_t; 
}


type rrep_hdr = {
  rrep_replier : Common.nodeid_t; (* node which sent the RREP. this is only
				     really used for logging purposes. *)
  rrep_dst : Common.nodeid_t; (* dest to which this rrep offers a route. *)
  rrep_orig : Common.nodeid_t;(* originator of rreq which elicited this rrep *)
}

type t = 
  | HELLO of seqno_t
  | DATA of str_hdr
  | RREQ of (str_hdr * rreq_hdr)
  | RREP of (str_hdr * rrep_hdr)

let hdr_size str_hdr = 
  match str_hdr with
    | HELLO _ -> seqno_size
    | DATA _ -> str_hdr_size
    | RREQ _ -> str_hdr_size + (2 * addr_size) + rreq_id_size 
    | RREP _ -> str_hdr_size + (3 * addr_size)
	

let clone str_pkt = str_pkt




