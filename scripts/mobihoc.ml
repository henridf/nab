(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

let nw_sizes = [
  500;
  1000;
  2000;
  4000;
  8000;
  16000;
]
let mobs = [
  new Mob.randomWalk;
  new Mob.waypoint
]
