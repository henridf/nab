





let initialseed = 12

let seed = ref initialseed
let change_seed ?newseed () = (
  begin 
    match newseed with 
      | None ->  seed := !seed + 14;
      | Some s -> seed := !seed + s;
  end;
  Random.init !seed
)

let rewind_seed() = 
  Random.init !seed


