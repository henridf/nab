open Printf

type 'a t = {
  mutable value: 'a option;
  name:string;
  doc:string;
  reader:(string -> 'a);
  checker:('a -> unit) option;
}

exception IllegalValue of string


let create ~name ~default ~doc ~reader ~checker =  (

  {value=default;
  name=name;
  doc=doc;
  reader=reader;
  checker=checker}
)
    
let set param value = (
  let _ = (
    match param.checker with
      | None -> ()
      | Some f -> (
	  try 
	    f value 
	  with  IllegalValue str -> 
	    raise(Misc.Fatal(Printf.sprintf "%s\n" str))
	)
  ) in
  param.value <- (Some value);
)

let strset param string = (
  let value = try 
    param.reader string 
  with e -> raise (Misc.Fatal 
    (Printf.sprintf "%s is not correct for param %s\n" string param.name))
  in
  set param value
)

let get param = match param.value with 
  | None -> raise (Misc.Fatal  
      (Printf.sprintf "Cannot provide value for param %s which had no default\n"  param.name))
  | Some value -> value











