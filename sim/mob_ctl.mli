(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Functions for controlling mobility processes *)

val make_waypoint_mobs : unit -> unit
  (** Create {!Mob.mobility} objects that implement a waypoint mobility
    model. *)

val make_epfl_waypoint_mobs : unit -> unit
  (** Create {!Mob.mobility} objects that implement a waypoint mobility model
    over epfl campus (uses {!Epflcoords.l}). *)

val make_discrete_randomwalk_mobs : unit -> unit
  (** Create {!Mob.mobility} objects that implement a discrete random walk 
    mobility model. *)

val start_node : Common.nodeid_t -> unit
  (** Starts mobility of given node. *)

val stop_node : Common.nodeid_t -> unit
  (** Stops mobility of given node. *)

val start_all : unit -> unit
  (** Starts mobility of all nodes. *)

val stop_all : unit -> unit
  (** Stops mobility of all nodes. *)
