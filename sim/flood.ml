(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

type t = Common.nodeid_t NaryTree.t
let create root  = NaryTree.Node (root, [])
let addnode = NaryTree.addnode
let to_coords = NaryTree.map ~f:(fun n -> (Gworld.world())#nodepos n)
