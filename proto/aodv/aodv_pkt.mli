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



(** AODV packet types and manipulators.
  As per Section 5 of RFC 3561. 
  
  @author Henri Dubois-Ferriere.
*)

type seqno_t = int


type rreq_flags = {
  g : bool; (** Grat RREP. *)
  d : bool; (** Destination only. *)
  u : bool; (** Unknown sequence number. *)
}

type rreq = {
  rreq_flags :       rreq_flags;
  rreq_hopcount :    int;
  rreq_id :          int;
  rreq_dst :         Common.nodeid_t;
  rreq_dst_sn :      seqno_t;
  rreq_orig :        Common.nodeid_t;
  rreq_orig_sn :     seqno_t;
}

type rrep_flags = {
  a  : bool; (** Acknowledgement required. *)
}

type rrep = {
  rrep_flags :        rrep_flags;
  prefix_size :       char;
  rrep_hopcount :     int;
  rrep_dst :          Common.nodeid_t;
  rrep_dst_sn :       seqno_t;
  rrep_orig :         Common.nodeid_t;
  lifetime :          float;
}



type rerr_flags = {
  nd : bool;      (** No delete. *)
}


type rerr = {
  (* no need to implement the destCount field, simply compute
     the List.length of the unreach field instead. *)
  rerr_flags : rerr_flags;
  unreach : (Common.nodeid_t * seqno_t option) list
}

type t =  
  | DATA
  | RREQ of rreq 
  | RREP of rrep 
  | RERR of rerr
  | RREP_ACK 

val hdr_size : t -> int

val clone : t -> t

val make_rrep_hdr : 
  ?flags:rrep_flags ->
  ?prefix_size:char -> 
  hc:int ->
  dst:Common.nodeid_t ->
  dst_sn:seqno_t ->
  orig:Common.nodeid_t ->
  lifetime:float -> unit -> 
  t

val make_rreq_hdr :
  ?flags:rreq_flags ->
  hc:int ->
  rreq_id:int ->
  rreq_dst:Common.nodeid_t ->
  rreq_dst_sn:seqno_t ->
  orig:Common.nodeid_t -> 
  orig_sn:seqno_t -> unit -> 
  t

val make_rerr_hdr :  
  ?flags : rerr_flags -> 
  (Common.nodeid_t * seqno_t option) list -> 
  t




