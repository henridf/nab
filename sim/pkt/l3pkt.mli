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







(** Layer 3 packet types and manipulators.
  A lot of the packet headers are protocol specific (see the corresponding
  files); here we only take care of the generic fields (src, dst, ..) and
  functions.
  
  @author Henri Dubois-Ferriere.
*)


(* todo: 
   possibly functions should take 'minimal' nec. data structure. 
   For example, set_l3ttl should not take a l3packet but a l3hdr. 
   Otherwise, how does a code segment which only has a l3hdr set the ttl?
*)

open L4pkt

(* ocamldocifying each and every function below is straightforward (if
   long..), not doing until more stability on how this should look *)


val l3_bcast_addr : int



(** {2 Network Layer (L3) Packet Types} *)

type l3hdr_ext_t = 
    [ `NONE
    | `LER_HDR of Ler_pkt.t 
    | `STR_HDR of Str_pkt.t
    | `AODV_HDR of Aodv_pkt.t
    | `DIFF_HDR of Diff_pkt.t
    | `SIMPLE_HDR of Simple_pkt.t
    | `DBF_HDR of Dbf_pkt.t
    ]

(** A [L3pkt.l3hdr_t] contains a src, dst, ttl and maybe some protocol-specific
   extensions *)
type l3hdr_t 

(** A [L3pkt.t] contains a l3hdr and a l4pkt *)
type t 


(** {2 Network Layer (L3) Packet Constructors} *)

val default_ttl : int

val make_l3hdr :
  src:Common.nodeid_t ->
  dst:Common.nodeid_t ->
  ?ttl:int -> 
  ?ext:l3hdr_ext_t -> unit -> 
  l3hdr_t
  (** default value for ttl is {!L3pkt.default_ttl} *)


val make_l3pkt : l3hdr:l3hdr_t -> l4pkt:L4pkt.t -> t

(** {2 Network Layer (L3) Packet Deconstructors} *)

val l4pkt : t -> L4pkt.t
val l3hdr : t -> l3hdr_t
val l3src : t -> Common.nodeid_t
val l3dst : t -> Common.nodeid_t
val l3ttl : t -> int



val l3hdr_ext : t -> l3hdr_ext_t
val ler_hdr : t -> Ler_pkt.t
val str_hdr : t -> Str_pkt.t
val aodv_hdr : t -> Aodv_pkt.t
val diff_hdr : t -> Diff_pkt.t
val simple_hdr : t -> Simple_pkt.t
val dbf_hdr : t -> Dbf_pkt.t

(** {2 L3 Header Manipulators} *)

val l3pkt_size : t -> int
val clone_l3pkt : t -> t

val decr_l3ttl : t -> t


