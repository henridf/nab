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

(** {2 Application Layer (L4) Packet Types}
  
  @author Henri Dubois-Ferriere.
*)

type hello_payload_t = Coord.coordf_t

type t =
    [ `EMPTY
    | `APP_PKT of int
    | `HELLO_PKT of hello_payload_t
    ]


(** {2 Application Layer (L4) Packet Constructors} *)

val clone_l4pkt : l4pkt:'a -> 'a


(** {2 Application Layer (L4) Packet Manipulators} *)

val l4pkt_size : l4pkt:t -> int
