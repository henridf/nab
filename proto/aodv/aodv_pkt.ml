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

(* Rem:
   if any mutables get introduced, remember to change clone_aodv_pkt
*)

open Pkt_common
open Misc

type seqno_t = int


type rreq_flags = {
  g : bool; (** Grat RREP. *)
  d : bool; (** Destination only. *)
  u : bool; (** Unknown sequence number. *)
}

type rreq = {
  rreq_flags :  rreq_flags;
  rreq_hop_count :   int;
  rreq_id :     int;
  rreq_dst :    Common.nodeid_t;
  rreq_dst_sn : seqno_t;
  rreq_orig :        Common.nodeid_t;
  rreq_orig_sn :     seqno_t;
}

type rrep_flags = {
  a  : bool; (** Acknowledgement required. *)
}

type rrep = {
  rrep_flags :   rrep_flags;
  prefix_size :  char;
  rrep_hop_count :    int;
  rrep_dst :     Common.nodeid_t;
  rrep_dst_sn :  seqno_t;
  rrep_orig :         Common.nodeid_t;
  lifetime :     int;
}

type rerr_flags = {
  n : bool (** No delete. *)
}

type rerr = {
  rerr_flags : rerr_flags;
  unreach : (Common.nodeid_t * seqno_t) list
}

type t =  
  | NONE 
  | RREQ of rreq 
  | RREP of rrep 
  | RERR of rerr
  | RREP_ACK 


let hdr_size aodv_hdr = 
  (* directly read off the written out packet formats in
     draft. *)
  match aodv_hdr with 
    | NONE -> 0
    | RREP_ACK -> 2
    | RREQ _ -> 24
    | RREP _ -> 20
    | RERR rerr -> 4 + 8 * (List.length rerr.unreach)


(* must change if any mutables get introduced!! *)
let clone aodv_pkt = aodv_pkt

let make_rrep_hdr 
  ?(flags = {a=false}) 
  ?(prefix_size = (char 0))
  ~hop_count
  ~dst
  ~dst_sn
  ~orig
  ~lifetime () 
  = 
  assert (hop_count >= 0 && lifetime > 0 );
  RREP {
    rrep_flags = flags;
    prefix_size = prefix_size;
    rrep_hop_count = hop_count;
    rrep_dst = dst;
    rrep_dst_sn = dst_sn;
    rrep_orig = orig;
    lifetime = lifetime;
  }


let make_rerr_hdr  ?(flags={n=false}) unreachables = 
  assert (List.length unreachables > 0);
  RERR {
    rerr_flags = flags;
    unreach = unreachables
  }
    

let make_rreq_hdr 
  ?(flags={g=false; d=false; u=false}) 
  ~hop_count 
  ~rreq_id 
  ~rreq_dst 
  ~rreq_dst_sn 
  ~orig 
  ~orig_sn () 
  = 
  assert (hop_count >= 0);
  RREQ {
    rreq_flags = flags;
    rreq_hop_count = hop_count;
    rreq_id = rreq_id;
    rreq_dst = rreq_dst;
    rreq_dst_sn = rreq_dst_sn;
    rreq_orig = orig;
    rreq_orig_sn = orig_sn;
  }
    

let rreq_orig_sn hdr = match hdr with 
  | RREQ h ->  h.rreq_orig_sn
  | NONE | RREP_ACK | RREP _ | RERR _ ->
      raise (Invalid_argument "Aodv_pkt.rreq_orig_sn")

let rreq_orig hdr = match hdr with 
  | RREQ h ->  h.rreq_orig
  | NONE | RREP_ACK | RREP _ | RERR _ ->
      raise (Invalid_argument "Aodv_pkt.rreq_orig")

let rreq_dst_sn hdr = match hdr with 
  | RREQ h ->  h.rreq_dst_sn
  | NONE | RREP_ACK | RREP _ | RERR _ ->
      raise (Invalid_argument "Aodv_pkt.rreq_dst_sn")

let rreq_dst hdr = match hdr with 
  | RREQ h ->  h.rreq_dst
  | NONE | RREP_ACK | RREP _ | RERR _ ->
      raise (Invalid_argument "Aodv_pkt.rreq_dst")
      

let rrep_orig hdr = match hdr with 
  | RREP h ->  h.rrep_orig
  | NONE | RREP_ACK | RREQ _ | RERR _ ->
      raise (Invalid_argument "Aodv_pkt.rrep_orig")

let rrep_dst_sn hdr = match hdr with 
  | RREP h ->  h.rrep_dst_sn
  | NONE | RREP_ACK | RREQ _ | RERR _ ->
      raise (Invalid_argument "Aodv_pkt.rrep_dst_sn")

let rrep_dst hdr = match hdr with 
  | RREP h ->  h.rrep_dst
  | NONE | RREP_ACK | RREQ _ | RERR _ ->
      raise (Invalid_argument "Aodv_pkt.rrep_dst")
      



let incr_hop_count hdr  = 
  match hdr with 
    | RREQ h -> RREQ { h with rreq_hop_count = h.rreq_hop_count + 1}
    | RREP h -> RREP { h with rrep_hop_count = h.rrep_hop_count + 1}
    | NONE | RREP_ACK  | RERR _ ->
	raise (Invalid_argument "Aodv_pkt.incr_hop_count")

let decr_hop_count hdr  = 
  match hdr with 
    | RREQ h -> 
	assert (h.rreq_hop_count > 0);
	RREQ { h with rreq_hop_count = h.rreq_hop_count - 1}
    | RREP h -> 
	assert (h.rrep_hop_count > 0);
	RREP { h with rrep_hop_count = h.rrep_hop_count - 1}
    | NONE | RREP_ACK  | RERR _ ->
	raise (Invalid_argument "Aodv_pkt.decr_hop_count")


