(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



  
class gpsnode : 
  pos_init:Coord.coordf_t -> 
  Common.nodeid_t ->
	  
object
  inherit Simplenode.simplenode
    
  method pos : Coord.coordf_t

end
  
	  
  
  
