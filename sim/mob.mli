(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

val make_waypoint_mobs : unit -> unit
val make_epfl_mobs : unit -> unit
val start_node : Common.nodeid_t -> unit
val stop_node : Common.nodeid_t -> unit
val start_all : unit -> unit
val stop_all : unit -> unit


(* xxx/hack copied from gui_hooks b/c otherwise makefile problems in using
   Gui_hooks.* from here *)
val x_mtr_to_pix : float -> int

val pos_pix_to_mtr : Coord.coordi_t -> Coord.coordf_t
val pos_mtr_to_pix : Coord.coordf_t -> Coord.coordi_t


class virtual mobility :
  string ->
  #Simplenode.simplenode ->
  (newpos:Coord.coordf_t -> unit) ->
  object
    val abbrev : string
    val mutable moving : bool
    val owner : #Simplenode.simplenode
    val mutable speed_mps : float
    method abbrevname : string
    method virtual getnewpos : gran:float -> Coord.coordf_t
    method move : unit
    method set_speed_mps : float -> unit
    method start : unit
    method stop : unit
  end

class waypoint :
  #Simplenode.simplenode ->
  (newpos:Coord.coordf_t -> unit) ->
  object
    inherit mobility
    method getnewpos : gran:float -> Coord.coordf_t
  end
