(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Last Encounter table for keeping track of last encounter times and places
  in LER algorithms (ie EASE, GREASE, etc).
*)



(** An encounter is a struct containing a position and a time. *)
type enc_t = {
  t: Common.time_t; 
  p: Coord.coordf_t
}


val enc : time:Common.time_t -> place:Coord.coordf_t -> enc_t
(*val enc_age : enc_t -> Common.time_t*)


type le_tab_state_t = enc_t option array 


class le_tab : ntargets:int -> 
object 

  method add_encounter : nid:Common.nodeid_t -> pos:Coord.coordf_t -> unit
    (** [add_encounter n pos] stores in the encounter table that we have
      just encountered node [n] at position [pos]. *)
    
  method le : nid:Common.nodeid_t -> enc_t option
    (** [le n] returns [Some enc], where enc is the {!enc_t}
      representing our last encounter with node [n], or returns [None] if we
      have never encountered [n].  *)
    
  method le_time : nid:Common.nodeid_t -> Common.time_t option
    (** Same as [le] above, except that it returns only the time of the last
      encounter. *)

  method le_pos : nid:Common.nodeid_t -> Coord.coordf_t option
    (** Same as [le] above, except that it returns only the position of 
      the last encounter. *)
    
  method le_age : nid:Common.nodeid_t -> Common.time_t 
    (** [le_age n] returns the age of our last encounter with node [n], or 
      [Pervasives.max_float] if we have never encountered [n].
    *)

  method num_encounters : int

  method dump_state : le_tab_state_t
    (* returns db state for marshalling. *)

  method load_state : le_tab_state_t -> unit
end
