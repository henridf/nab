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
  establishments for Last-encounter routing protocols (ie FRESH or GREASE).

  @author Henri Dubois-Ferriere
*)


type 'anchor info_t = 
    {anchor: 'anchor; 
    anchor_age: Time.time_t; 
    searchcost: float}
    (**
      The info type attached to hops in a EASE/GREASE route.
      The anchor changes at the hop which does a new anchor search (or in the
      case of GREASE, at the hop which locally has a better anchor, in which
      case the searchcost will be 0.)
      The anchor_age represents the age of the current anchor, and also changes 
      when the anchor changes.
      The searchcost can only be non-zero at hops where the anchor is
      different than the previous hop.
    *)
    
type ('hop, 'anchor) t = ('hop, 'anchor info_t) Route.t
    (** A EASE/GREASE route, using [info_t] for additional info. 
      Note this type is still polymorphic in ['a], to allow representing the
      hops either as node ids or as geographical positions (useful in a GUI).*)

val ler_route_valid : ('hop, 'anchor) t -> src:'hop -> dst:'hop -> bool
  (* generic checks:
     - length >= 1
     - searchcost >= 0
     - searchcost can only be non-zero when anchor changes
     - anchor_age must be monotonically decreasing
     - anchor_age can only change when anchor changes
     - starts at src, ends at dst
     - is loop-free (no hop is repeated twice)
  *)

val search_cost : ('a, 'b) t -> float 
  (* Sum of squares of all search radii *)

val sprintnid : (Common.nodeid_t, Coord.coordf_t) t -> string  
