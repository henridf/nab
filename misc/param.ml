type 'a t = {
  mutable value: 'a;
  name:string;
  doc:string;
  reader:(string -> 'a);
  checker:('a -> unit) option;
}

exception IllegalValue of string

let create ~name ~default ~doc ~reader ~checker =  
  {value=default;
  name=name;
  doc=doc;
  reader=reader;
  checker=checker}
  
    
let set param value = (
  let _ = (
    match param.checker with
      | None -> ()
      | Some f -> (
	  try 
	    f value 
	  with  IllegalValue str -> 
	    raise(Util.Fatal(Printf.sprintf "%s\n" str))
	)
  ) in
  param.value <- value;
)

let strset param string = (
  let value = try 
    param.reader string 
  with e -> raise (Util.Fatal 
    (Printf.sprintf "%s is not correct for param %s\n" string param.name))
  in
  set param value
)

let get param = param.value
