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







(** {2 Application Layer (L4) Packet Types}
  
  @author Henri Dubois-Ferriere.
*)

type hello_payload_t = Coord.coordf_t




type t =
    [ `NONE
    | `APP_PKT
    | `HELLO_PKT of hello_payload_t
    ]


(** {2 Application Layer (L4) Packet Constructors} *)

val clone_l4pkt : l4pkt:'a -> 'a


(** {2 Application Layer (L4) Packet Manipulators} *)

val l4pkt_size : l4pkt:t -> int
