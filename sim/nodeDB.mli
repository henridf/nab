type nodeDB_state_t = Common.enc_t option array 
class type nodeDB_t = 
object 
  
  method add_encounter : nid:Common.nodeid_t -> enc:Common.enc_t -> unit
    
  method last_encounter : nid:Common.nodeid_t -> Common.enc_t option
    (* None if never met *)
    
  method encounter_age : nid:Common.nodeid_t -> Common.time_t 

  method num_encounters : int

  method dump_state : node_cnt:int ->  nodeDB_state_t
    (* returns db state for marshalling. Array size will be nodes_cnt *)

  method load_state : dbstate:nodeDB_state_t -> unit

end

class nodeDB : ntargets:int -> nodeDB_t
