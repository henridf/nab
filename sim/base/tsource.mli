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







(** Traffic Generators. 
  @author Henri Dubois-Ferriere.
*)

val make_cbr : ?num_pkts:int -> pkts_per_sec:float -> unit -> Trafficgen.t
  (** Returns a traffic generator which originates packets, at a fixed rate
    of [pkts_per_sec]. If [num_pkts] is provided, traffic generator will stop
    after [num_pkts], otherwise generates packets indefinitely. *)

val make_poisson :  ?num_pkts:int -> lambda:float -> unit -> Trafficgen.t 
  (** Returns a traffic generator which will originate num_pkts, spaced
    with an exponential distribution of parameter lambda (mean 1/lambda).
    If [num_pkts] is provided, traffic generator will stop after [num_pkts],
    otherwise generates packets indefinitely.
  *)
