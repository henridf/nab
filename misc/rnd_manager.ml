(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



let initialseed = 12

let seed = ref initialseed
let change_seed() = (
  seed := !seed + 14;
  Random.init !seed
)

let init_seed() = (
  seed := initialseed;
  Random.init !seed
)
