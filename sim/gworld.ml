(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc

let (world_:World.world_t option ref) = ref None 
let world () = try o2v !world_ with
  | Failure _ -> raise (Failure "Gworld.world() : no instance has been set")

let set_world t = world_ := Some t
