(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)



open Pkt_common

(* L3 STUFF *)
type aodv_flags_t = 
    AODV_DATA | AODV_RREQ | AODV_RREP | AODV_RERR | AODV_RADV

type t = {
  mutable aodv_flags : aodv_flags_t;
  ssn : int;         (* Source Seqno: All *)
  dsn : int;         (* Destination Seqno: RREQ *)
  mutable shc : int; (* Source hopcount: All *)
  mutable dhc : int; (* Destination hopcount: RREQ *)
  osrc : Common.nodeid_t; (* OBO Source: RREP *)
  osn : int;              (* OBO Seqno: RREP *)
  ohc : int;      (* OBO Hopcount: RREP *)
  rdst : Common.nodeid_t; (* Route request destination : RREQ *)
}

let hdr_size pkt = (* too lazy to differentiat btw types right now, so
			     just putting the 'average' size *)
      1  (* aodv_flags *)
      + (2 * _SEQNO_SIZE)  (* ssn, dsn *)
      + (2 * _TTL_SIZE) (* shc, dhc *)

let clone aodv_pkt = {aodv_pkt with ssn=aodv_pkt.ssn}


let flags aodv_pkt = 
  aodv_pkt.aodv_flags

let ssn aodv_pkt = aodv_pkt.ssn
let shc aodv_pkt = aodv_pkt.shc
let dsn aodv_pkt = let v = aodv_pkt.dsn in assert (v <> -1); v
let dhc aodv_pkt = let v = aodv_pkt.dhc in assert (v <> -1); v
let osrc aodv_pkt = let v = aodv_pkt.osrc in assert (v <> -1); v
let ohc aodv_pkt = let v = aodv_pkt.ohc in assert (v <> -1); v
let osn aodv_pkt = let v = aodv_pkt.osn in assert (v <> -1); v
let rdst aodv_pkt = let v = aodv_pkt.rdst in assert (v <> -1); v


let incr_shc_pkt aodv_pkt  = 
  aodv_pkt.shc <- aodv_pkt.shc + 1

let decr_shc_pkt aodv_pkt  = 
  aodv_pkt.shc <- aodv_pkt.shc - 1

let make_aodv_hdr 
  ?(dhc = -1)
  ?(dsn= -1)
  ?(osrc= -1)
  ?(ohc= -1)
  ?(osn= -1)
  ?(rdst= -1) 
  ~flags 
  ~ssn 
  ~shc
  ()
  = (
    begin 
      match flags with 
	| AODV_DATA -> 
	    assert (dhc = -1 && dsn = -1  && osrc = -1 && 
    ohc = -1 && osn = -1 && rdst = -1)
	| AODV_RREQ | AODV_RERR ->
	    assert (ohc = -1 && osn = -1 && osrc = -1 && 
    rdst <> -1 && dhc <> -1 && dsn <> -1)
	| AODV_RREP ->
	    assert (ohc <> -1 && osn <> -1 && osrc <> -1 && 
	    rdst = -1 && dhc = -1 && dsn = -1)
	| AODV_RADV  -> 
	    assert (ohc = -1 && osn = -1 && osrc = -1 && 
	    rdst = -1 && dhc = -1 && dsn = -1)
    end;
    {
      aodv_flags=flags;
      ssn=ssn;
      shc=shc;
      dhc=dhc;
      dsn=dsn;
      ohc=ohc;
      osn=osn;
      osrc=osrc;
      rdst=rdst
    }
  )
