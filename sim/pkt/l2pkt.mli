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

(** Layer 2 packet types and manipulators.
  
  @author Henri Dubois-Ferriere.
*)
open L3pkt

val l2_bcast_addr : int


(** {2 L2 Packet Types} *)

type l2hdr_ext_t = 
    NONE | MACAW of Macaw_pkt.t | MACA of Maca_pkt.t

type l2hdr_t (** A L2 header contains a src, dst and maybe some MAC-specific
	       extensions. *)


type t (** The type of a layer 2 packet, which contains a l2 header and a
	 l3pkt. *)


(** {2 L2 Packet Constructors} *)

val make_l2pkt :
  ?ext:l2hdr_ext_t -> src:Common.nodeid_t -> dst:Common.nodeid_t -> L3pkt.t -> t
val clone_l2pkt : l2pkt:t -> t



(** {2 L2 Packet Deconstructors} *)

val l3pkt : t -> L3pkt.t
(*val l2hdr : t -> l2hdr_t*)
val l2src : t -> Common.nodeid_t
val l2dst : t -> Common.nodeid_t
val string_of_l2dst : Common.nodeid_t -> string

val l2hdr_ext : t -> l2hdr_ext_t
val maca_hdr : t -> Maca_pkt.t


(** {2 Packet Sizes} *)

val l2pkt_size : l2pkt:t -> int
