type nodeDB_state_t = Common.enc_t option array 
class type nodeDB_t = 
object 
  
  method add_encounter : nid:Common.nodeid_t -> enc:Common.enc_t -> unit
    
  method last_encounter : nid:Common.nodeid_t -> Common.enc_t option
    (* None if never met *)
    
  method encounter_age : nid:Common.nodeid_t -> Common.time_t 
    (* max_float if never met *)
  method num_encounters : int

  method dump_state : nodeDB_state_t
    (* returns db state for marshalling. *)

  method load_state : nodeDB_state_t -> unit

end

class nodeDB : ntargets:int -> nodeDB_t
