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







(** Last Encounter table for keeping track of last encounter times and places
  in LER algorithms (ie EASE, GREASE, etc).
*)



(** An encounter is a struct containing a position and a time. *)
type enc_t = {
  t: Time.time_t; 
  p: Coord.coordf_t
}


val enc : time:Time.time_t -> place:Coord.coordf_t -> enc_t
(*val enc_age : enc_t -> Time.time_t*)


type le_tab_state_t = enc_t option array 


class le_tab : ntargets:int -> 
object 

  method add_encounter : nid:Common.nodeid_t -> pos:Coord.coordf_t -> unit
    (** [add_encounter n pos] stores in the encounter table that we have
      just encountered node [n] at position [pos]. *)
    
  method le : nid:Common.nodeid_t -> enc_t option
    (** [le n] returns [Some enc], where enc is an [enc_t]
      representing our last encounter with node [n], or returns [None] if we
      have never encountered [n].  *)
    
  method le_time : nid:Common.nodeid_t -> Time.time_t option
    (** Same as [le] above, except that it returns only the time of the last
      encounter. *)

  method le_pos : nid:Common.nodeid_t -> Coord.coordf_t option
    (** Same as [le] above, except that it returns only the position of 
      the last encounter. *)
    
  method le_age : nid:Common.nodeid_t -> Time.time_t 
    (** [le_age n] returns the age of our last encounter with node [n], or 
      [Pervasives.max_float] if we have never encountered [n].
    *)

  method num_encounters : int

  method dump_state : le_tab_state_t
    (* returns db state for marshalling. *)

  method load_state : le_tab_state_t -> unit
end
