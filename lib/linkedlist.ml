

(* each node has a value, and a foreward pointer *)
type 'a node = { 
  mutable v : 'a;
  mutable fore : 'a node option 
}

type 'a llist =  'a node option ref
    
let create () = ref None


let insert ~ll ~v ~compare = 
  let rec advance node = 
    match node.fore with
      | None -> node.fore <- Some {v=v; fore=None}
      | Some nextnode -> 
	  match compare  v nextnode.v with
	    | true -> node.fore <- Some {v=v; fore=Some nextnode}
	    | false -> advance nextnode
  in
  match !ll with 
    | None -> ll := Some {v=v; fore=None} (* empty *)
    | Some head -> 
	match compare v head.v with (* One node *)
	  | true -> ll := Some {v=v; fore = Some head};
	  | false -> advance head (* > 1 node *)

let iter f ll = 
  let rec advance node = 
    f node.v;
    match node.fore with
      | None -> ()
      | Some nextnode -> 
	  advance nextnode
  in 
  match !ll with 
    | None -> () 
    | Some head ->
	advance head


let pophead ~ll = 
match !ll with 
  | None -> None   (* empty *)
  | Some head -> ( 
      match head.fore with
	| None -> ll := None (* One node *)
	| Some next -> ll := Some {v=next.v; fore=next.fore}; (* > 1 node *)
    );
      Some head.v

  
