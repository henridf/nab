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

(* used for keeping track of which params 
   are argspeccable *)
let intparams = ref []
let floatparams = ref []
let stringparams = ref []



exception IllegalValue of string

let create ~name  ?shortname ?(cmdline=false) ?default ~doc ~reader ?checker () =  (

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
    
let intcreate ~name ?shortname ?(cmdline=false) ?default ~doc ?checker () = 
  let p = create ~name ?shortname ~cmdline ?default ~doc ~reader:int_of_string
    ?checker () in
  if cmdline then intparams := p::!intparams;
  p

let floatcreate ~name ?shortname ?(cmdline=false) ?default ~doc ?checker () = 
  let p = create ~name ?shortname ~cmdline ?default ~doc ~reader:float_of_string
    ?checker () in
  if cmdline then floatparams := p::!floatparams;
  p

let boolcreate ~name ?shortname ?(cmdline=false) ?default ~doc ?checker () = 
  create ~name ?shortname ~cmdline ?default ~doc ~reader:bool_of_string
    ?checker () 

let stringcreate ~name ?shortname ?(cmdline=false) ?default ~doc ?checker () = 
  let p = create ~name ?shortname ~cmdline ?default ~doc ~reader:(fun s -> s)
    ?checker () in
  if cmdline then stringparams := p::!stringparams;
  p


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
	Myarg.Int (fun i -> set param i),
	param.doc)

let make_floatargspec param = 
	("-"^param.shortname, 
	Myarg.Float (fun i -> set param i),
	param.doc)

let make_stringargspec param = 
	("-"^param.shortname, 
	Myarg.String (fun i -> set param i),
	param.doc)

let make_argspeclist () = 
  List.map (fun p -> make_intargspec p) !intparams
  @
  List.map (fun p -> make_floatargspec p) !floatparams
  @
  List.map (fun p -> make_stringargspec p) !stringparams
    

let dumpconfig () = 
  List.sort (fun (a, _) (b, _) -> String.compare a b)
    (List.map (fun p -> (p.doc, (Misc.o2v p.value))) !stringparams
    @
    List.map (fun p -> (p.doc, string_of_float (Misc.o2v p.value))) !floatparams
    @
    List.map (fun p -> (p.doc, string_of_int (Misc.o2v p.value))) !intparams)
    
