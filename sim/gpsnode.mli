(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



  
class gpsnode : 
  ?pos_init:Coord.coordf_t -> 
  Common.nodeid_t ->
	  
object
  inherit Simplenode.simplenode
    
    
  method pos : Coord.coordf_t

  method move : newpos:Coord.coordf_t -> unit

end
  
	  
(*
class node : 
  pos_init:Coord.coordf_t -> 
  id:Common.nodeid_t ->
  node_t
*)
  
  
