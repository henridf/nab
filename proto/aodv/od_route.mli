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

(** Data structure and assorted functions representing routes and route
  establishments for on-demand distance-vector  protocols (ie AODV or GREP).

  @author Henri Dubois-Ferriere
*)




type info_t = Flood.t
    (** The optional info type attached to a GREP route is a {!Flood.t}
      object, which represents the (flooded) search which some nodes
      perform. At hops where no flood was necessary, the [hop_t.info] field is
      set to [None]. *)

type 'a t = ('a, info_t) Route.t
    (** The type of GREP routes. *)

