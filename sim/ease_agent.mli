(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** EASE and GREASE Routing Agents.
 
  This is a simple implementation of [GR]EASE which takes some global shortcuts,
  ie it "cheats" compared to a purely distributed implementation.

  These shortcuts are:

  - Geographical Routing: When we are at position X, and have a packet
  addressed to, position Y, the next hop is chosen as the next closest node to
  y (after ourselves). This simple algorithm is guaranteed we will arrive at the
  closest node to point Y without getting into dead-ends, etc.

  - Anchor Search: This is found by using {!Worldt.lazy_world_t.find_closest}, ie by
  searching globally (as opposed to flooding a real search packet with an
  expanding ring search, etc).

  - Neighbor Notification: We use {!Worldt.greedy_world_t.add_new_ngbr_hook}
  to be instantaneously notified each time a node comes into range (as opposed
  to sending and listening for periodic hello packets).

  @author Henri Dubois-Ferriere.
 *)


(** Pass [true] for [grease] argument to constructor to get a GREASE agent,
  [false] to get EASE. *)
class ease_agent : ?stack:int -> grease:bool -> #Simplenode.simplenode -> 
object 
  inherit Log.inheritable_loggable
  inherit Rt_agent.t

  method le_tab : NodeDB.le_tab
  method set_le_tab : NodeDB.le_tab -> unit
end

val set_agents : ease_agent array -> unit
val agent : int -> ease_agent
val proportion_met_nodes : unit -> float


