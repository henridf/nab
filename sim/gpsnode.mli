(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



  
class gpsnode : 
  pos_init:Coord.coordf_t -> 
  id:Common.nodeid_t ->
	  
object
  inherit Simplenode.simplenode
    
    
  method pos : Coord.coordf_t

  method move : newpos:Coord.coordf_t -> unit

  method add_mob_mhook : hook:(Coord.coordf_t -> gpsnode -> unit) -> unit
    (* Any monitoring application can register here to receive update each
       time the node moves.
       If multiple apps, order in which called is unspecified.*)
    
end
  
	  
(*
class node : 
  pos_init:Coord.coordf_t -> 
  id:Common.nodeid_t ->
  node_t
*)
  
  
