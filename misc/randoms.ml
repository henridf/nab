(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)



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


type rng_stream = {mutable state: Random.state}

let save_state_ handle = 
  handle.state<-Random.get_state()

let create ?seed () = 
  match seed with 
    | None -> {state=Random.get_state()}
    | Some s -> Random.init s; {state=Random.get_state()}
	
let stream_ handle f = 
  let s = Random.get_state () in
  Random.set_state handle.state; 
  let number = f() in
  save_state_ handle;
  Random.set_state s;
  number

let int handle i =
  stream_ handle (fun () -> Random.int i) 
  
let float handle x = 
  stream_ handle (fun () -> Random.float x) 

let bool handle = 
  stream_ handle (fun () -> Random.bool ()) 

