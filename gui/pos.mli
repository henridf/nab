(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* need some sort of API for gui to get positions.*)

(* this api should be general enough to hide many possible models:
   - gui receives positions 'live' from mhooks, must store them in memory
   - positions are stored in a file 
   (either because gui is in playback mode,
   or because positions are read from a file by mws itself)
*)   


(* gui only knows positions in pixels *)
(* translation from meters should be done within the mhooks *)

(* returns the node's position at given time *)
val node_pos_inst : 
  Common.nodeid_t -> 
  Common.time_t -> 
  Common.time_t * Coord.coordi_t

val node_pos_range : 
  Common.nodeid_t -> 
  start:Common.time_t ->
  finish:Common.time_t ->
  (Common.time_t * Coord.coordi_t) list 
  (* why list? array would be no use. since time is not directly dependent on
     teh index, we need to scan 1by1 anyway to get through the list. 
     if ranges become too large for this to be efficient, we should use
     something like a heap or tree or whatever.*)

val nodes_pos_inst : 
  Common.nodeid_t array -> 
  Common.time_t -> 
  (Common.time_t * Coord.coordi_t) array

val nodes_pos_range : 
  Common.nodeid_t array -> 
  start:Common.time_t ->
  finish:Common.time_t ->
  (Common.time_t * Coord.coordi_t) list array 

val enter_node_pos : 
  (Common.nodeid_t * Coord.coordi_t) ->
  unit

val enter_nodes_pos : 
  (Common.nodeid_t * Coord.coordi_t) array ->
  unit
