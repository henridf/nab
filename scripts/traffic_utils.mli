(*
 * 
 * NAB - Network in a Box Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 * Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 * Laboratory for Computer Communications and Applications (LCA), Ecole
 * Polytechnique Federale de Lausanne (EPFL), CH-1015 Lausanne, Switzerland
 * 
 * This file is part of NAB. NAB is free software; you can redistribute it
 * and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 * 
 * NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details (enclosed in the file GPL).
 * 
 *)

(* $Id$ *)

(** Utilities for configuring and setting up traffic patterns. *)

(** The type of traffic matrices. *)
type trafficmatrix = 
  | HOTSPOT (** All nodes send to the same destination. *)
  | BIDIR   (** Bidirectional traffic between different node pairs. *)
  | UNIDIR  (** Bidirectional traffic between different node pairs. *) 


val hotspot_dst : unit -> Common.nodeid_t
  (** The "hot" node when using a [HOTSPOT] traffic matrix. *)

module TParams :
  sig
    val tmat : trafficmatrix Param.t
      (** Configuration parameter for the traffic matrix. *)
    val sources : int Param.t
      (** Configuration parameter for the number of sourcs. *)
    val rate : float Param.t
      (** Configuration parameter for the packet injection rate at each
	source. *)
    val pkts_orig : int Param.t
      (** Configuration parameter for the total number of packets to be
	originated. *)
  end

val install_tsources : unit -> unit
  (** Install CBR (see {!Tsources.make_cbr} traffic generators configured to
    give the traffic matrix according to the current configuration values in
    {!Traffic_utils.TParams}.
    The traffic generators on every source are programmed to start at random
    time uniformly chosen over an interval [0, 1/rate] (where rate is the
    value of {!Tparams.rate}), so that sources do not originate packets all at
    once when the simulation starts.
  *)

val clear_tsources : unit -> unit
  (** Clear all traffic sources. *)

val is_source : Common.nodeid_t -> bool
  (** [is_source nid] returns true if node [nid] is a traffic source according
    to the current configuration values in {!Traffic_utils.TParams}.*)

val all_sources : unit -> Common.nodeid_t list
  (** Returns a list of all nodes which are sources according to the current
    configuration values in {!Traffic_utils.TParams}.*)

val all_destinations : unit -> Common.nodeid_t list
  (** Returns a list of all nodes which are destinations according to the
    current configuration values in {!Traffic_utils.TParams}.*)


val dests_of_source : Common.nodeid_t -> Common.nodeid_t list option
  (** [dests_of_source nid] returns [Some l] where [l] is the list of
    destinations that [nid] would source packets to (according to the current
    configuration values in {!Traffic_utils.TParams}), or [None] if [nid] is
    not a packet source. *)

val src_dst_pairs : unit -> (Common.nodeid_t * Common.nodeid_t list) list
  (** Returns a list of pairs [(src, dests)] where [dests] is the list of
    destinations to which [src] sends packets (according to the current
    configuration values in {!Traffic_utils.TParams}). *)

val sprint_matrix : unit -> string
  (** Returns the traffic matrix in a human-readable form. *)

