(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

let initial_hash_size = 1000

type nodeDB_state_t = Common.enc_t option array 

class type nodeDB_t = 
object 
  method add_encounter : nid:Common.nodeid_t -> enc:Common.enc_t -> unit
  method last_encounter : nid:Common.nodeid_t -> Common.enc_t option
  method encounter_age : nid:Common.nodeid_t -> Common.time_t 
  method num_encounters : targets:int -> int
  method dump_state : node_cnt:int ->  nodeDB_state_t
  method load_state : dbstate:nodeDB_state_t -> unit
end


class nodeDB : nodeDB_t = 
object(s)

  val hash = Hashtbl.create 1000

  method add_encounter ~nid:nid ~enc:enc = 
    Hashtbl.replace hash nid enc
      
  method last_encounter ~nid =  (
    try 
      Some (Hashtbl.find hash nid)
    with
	Not_found -> None
  )
    
  method encounter_age ~nid = 
    match s#last_encounter ~nid:nid with
	None -> max_float
      | Some encounter -> Common.enc_age encounter

  method num_encounters ~targets = 
    Hashtbl.fold 
      (fun k v c -> c + if k < targets then 1 else 0) 
      hash 0

  method dump_state ~node_cnt = 
    Array.init node_cnt (fun i -> s#last_encounter i)

  method load_state ~dbstate = 
    Array.iteri
      (fun i encopt -> 
	match encopt with
	  | None -> ()
	  | Some enc -> s#add_encounter ~nid:i ~enc:enc
      ) dbstate
end


