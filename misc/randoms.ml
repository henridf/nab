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

type handle = {mutable state: Random.state}

let save_state_ handle = 
  handle.state<-Random.get_state()

let create ?seed () = 
  match seed with 
    | None -> {state=Random.get_state()}
    | Some s -> Random.init s; {state=Random.get_state()}
	
  
let int handle i = 
  Random.set_state handle.state; 
  let number = Random.int i in
  save_state_ handle;
  number

  
let float handle float = 
  Random.set_state handle.state; 
  let number = Random.float float in
  save_state_ handle;
  number

let bool handle = 
  Random.set_state handle.state; 
  let bool = Random.bool () in
  save_state_ handle;
  bool
