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
let boolparams = ref []



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
  
  begin
    match checker, default with 
      | Some f, Some v -> f v
      | _ -> ();
  end;
  {value=default;
  shortname=(match shortname with None -> name | Some n -> n);
  name=name;
  doc=doc;
  reader=reader;
  checker=checker}
)
    
let intcreate ~name ?shortname ?(cmdline=false) ?default ~doc ?checker () = 
  let p = create ~name ?shortname ?default ~doc ~reader:int_of_string
    ?checker () in
  if cmdline then intparams := p::!intparams;
  p

let floatcreate ~name ?shortname ?(cmdline=false) ?default ~doc ?checker () = 
  let p = create ~name ?shortname ?default ~doc ~reader:float_of_string
    ?checker () in
  if cmdline then floatparams := p::!floatparams;
  p

let boolcreate ~name ?shortname ?(cmdline=false) ?default ~doc ?checker () = 
  let p = create ~name ?shortname ?default ~doc ~reader:bool_of_string
    ?checker () in
  if cmdline then boolparams := p::!boolparams;
  p

let stringcreate ~name ?shortname ?(cmdline=false) ?default ~doc ?checker () = 
  let p = create ~name ?shortname ?default ~doc ~reader:(fun s -> s)
    ?checker () in
  if cmdline then stringparams := p::!stringparams;
  p


let set param value = (

  begin match param.checker with
      | None -> ()
      | Some f -> (
	  try 
	    f value 
	  with  IllegalValue str -> 
	    raise(Misc.Fatal(Printf.sprintf "%s\n" str))
	)
  end;	  
  param.value <- (Some value);
)

let strset param string = (
  let value = try 
    param.reader string 
  with _ -> raise (Misc.Fatal 
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

let make_stringargspec param = 
	("-"^param.shortname, 
	Arg.String (fun i -> set param i),
	param.doc)

let make_boolargspec param = 
	("-"^param.shortname, 
	Arg.Bool (fun i -> set param i),
	param.doc)

let make_argspeclist () = 
  List.map (fun p -> make_intargspec p) !intparams
  @
  List.map (fun p -> make_floatargspec p) !floatparams
  @
  List.map (fun p -> make_stringargspec p) !stringparams
  @
  List.map (fun p -> make_boolargspec p) !boolparams
    
let get_value p = 
  match p.value with 
    | None -> raise (Failure ("No value for parameter "^p.name^" "^p.doc))
    | Some v -> v

let configlist () = 
  List.sort (fun (a, _) (b, _) -> String.compare a b)
    (List.map (fun p -> (p.name, (get_value p))) !stringparams
    @
    List.map (fun p -> (p.name, string_of_float (get_value p))) !floatparams
    @
    List.map (fun p -> (p.name, string_of_int (get_value p))) !intparams
    @
    List.map (fun p -> (p.name, string_of_bool (get_value p))) !boolparams)

let configlist_doc () = 
  List.sort (fun (a, _) (b, _) -> String.compare a b)
    (List.map (fun p -> (p.doc, (get_value p))) !stringparams
    @
    List.map (fun p -> (p.doc, string_of_float (get_value p))) !floatparams
    @
    List.map (fun p -> (p.doc, string_of_int (get_value p))) !intparams
    @
    List.map (fun p -> (p.doc, string_of_bool (get_value p))) !boolparams)
    
  
let printconfig outchan = (
  let configlist = configlist_doc () in

  let max_width = 
    List.fold_left 
      (fun w (name,_) -> 
	if String.length name > w then String.length name else w)
      0
      configlist
  in

  List.iter (fun (name, value) ->
    output_string outchan ((Misc.padto name (max_width + 4))^value^"\n")
  ) configlist;
  flush outchan;
)


let sprintconfig () = (
  let configlist = configlist_doc () in

  let max_width = 
    List.fold_left 
      (fun w (name,_) -> 
	if String.length name > w then String.length name else w)
      0
      configlist
  in

  let stringlist = 
    List.map (fun (name, value) ->
      (Misc.padto name (max_width + 4))^value^"\n"
    ) configlist 
  in
  List.fold_left (^) "" stringlist;

)
