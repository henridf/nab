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




(** Traffic Generators. 
  @author Henri Dubois-Ferriere.
*)

val make_cbr : ?num_pkts:int -> rate:float -> unit -> Trafficgen.t
  (** Returns a traffic generator which originates packets, at a fixed rate
    of [rate] packets/second. If [num_pkts] is provided, traffic generator will stop
    after [num_pkts], otherwise generates packets indefinitely. *)

val make_cbr_uniform_jitter : ?num_pkts:int -> interval:float -> jitter:float
  -> unit -> Trafficgen.t
  (** Returns a traffic generator which originates one packets, in every
    interval of duration [interval] seconds.
    Within an interval, the exact packet origination time is not
    deterministic; it is chosen according to a uniform distribution centered
    around [-jitter, jitter] ( [jitter] <= [interval]).
    
    If [num_pkts] is provided, traffic generator will stop
    after [num_pkts], otherwise generates packets indefinitely. *)


val make_poisson :  ?num_pkts:int -> lambda:float -> unit -> Trafficgen.t 
  (** Returns a traffic generator which will originate num_pkts, spaced
    with an exponential distribution of parameter lambda (mean 1/lambda).
    If [num_pkts] is provided, traffic generator will stop after [num_pkts],
    otherwise generates packets indefinitely.
  *)
