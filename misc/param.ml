open Printf

let namespace = Hashtbl.create 50
let parambag = Hashtbl.create 50



type 'a t = {
  mutable value: 'a option;
  name:string;
  shortname:string;
  doc:string;
  reader:(string -> 'a);
  checker:('a -> unit) option;
}

type paramtype = Intparam | Floatparam 


exception IllegalValue of string



let create ~name  ?shortname ?default ~doc ~reader ?checker () =  (

  if Hashtbl.mem namespace name then 
    raise (Failure (Printf.sprintf "Param.create : %s already taken" name));
  Hashtbl.add namespace name name;
  
  if shortname <> None then (
    let sn = (Misc.o2v shortname) in
    if Hashtbl.mem namespace sn then 
      raise (Failure (Printf.sprintf "Param.create : %s already taken" sn));
    Hashtbl.add namespace sn sn;
  );
  
  {value=default;
  shortname=(match shortname with None -> name | Some n -> n);
  name=name;
  doc=doc;
  reader=reader;
  checker=checker}
)
    
let intcreate  =   create ~reader:int_of_string 
let floatcreate =  create  ~reader:float_of_string


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



let make_intargspec param = 
	("-"^param.shortname, 
	Arg.Int (fun i -> set param i),
	param.doc)

let make_floatargspec param = 
	("-"^param.shortname, 
	Arg.Float (fun i -> set param i),
	param.doc)
