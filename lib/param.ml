(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)



open Printf

let namespace = Hashtbl.create 50
let parambag = Hashtbl.create 50


type 'a t = {
  mutable value: 'a option;
  name:string;
  doc:string;
  reader:(string -> 'a);
  printer:('a -> string);
  checker:('a -> unit) option;
}


let params_speclist : (Arg.key * Arg.spec * Arg.doc) list ref = ref []
  (* contains all params *)

let params_configlist : (unit -> string * string) list ref = ref []
  (* for all params, contains a function which returns the name field and the
     value (as_string) of the param. *)

let params_configlist_doc : (unit -> string * string) list ref = ref []
  (* for cmdline params only, contains a function which returns the doc field
     and the  value (as_string) of the param. *)

let allparams = ref []

exception IllegalParamVal of string
exception NoParamVal of string

let set param value = (

  begin match param.checker with
      | None -> ()
      | Some f -> (
	  try 
	    f value 
	  with  IllegalParamVal str -> 
	    raise (IllegalParamVal str)
	)
  end;	  
  param.value <- (Some value);
)

let get_value p = 
  match p.value with 
    | None -> raise (Failure ("No value for parameter "^p.name^" "^p.doc))
    | Some v -> v

let get param = match param.value with 
  | None -> raise (NoParamVal
      (Printf.sprintf "Cannot provide value for param %s which had no default\n"  param.name))
  | Some value -> value

let as_string p = p.printer (get p)

let make_intargspec param = 
  ("-"^param.name, 
  Arg.Int (fun i -> set param i),
  param.doc)
  
let make_floatargspec param = 
  ("-"^param.name, 
  Arg.Float (fun i -> set param i),
  param.doc)

let make_stringargspec param = 
  ("-"^param.name, 
  Arg.String (fun i -> set param i),
  param.doc)

let make_boolargspec param = 
  ("-"^param.name, 
  Arg.Bool (fun i -> set param i),
  param.doc)

let make_otherargspec param = 
  ("-"^param.name, 
  Arg.String (fun i -> set param (param.reader i)),
  param.doc)

let register ~name ?default ~doc ~reader ~printer ?checker () =  (

  if Hashtbl.mem namespace name then 
    raise (Failure (Printf.sprintf "Param.register : %s already taken" name));
  Hashtbl.add namespace name name;
  
  begin
    match checker, default with 
      | Some f, Some v -> f v
      | _ -> ();
  end;
  {value=default;
  name=name;
  doc=doc;
  reader=reader;
  printer=printer;
  checker=checker}
)

let create ~name ~doc ~reader ~printer ?(cmdline=false) ?default ?checker () = 
  let p = register ~name ~printer ?default ~doc ~reader ?checker () in
  if cmdline then (
    params_speclist := (make_otherargspec p)::!params_speclist;
    params_configlist_doc := (fun () -> (p.doc, (as_string p)))::!params_configlist_doc
  );
  params_configlist := (fun () -> (p.name, (as_string p)))::!params_configlist;
  p

    
let intcreate ~name ?(cmdline=false) ?default ~doc ?checker () = 
  let p = register ~name ?default ~doc ~reader:int_of_string ~printer:string_of_int
    ?checker () in
  if cmdline then (
    params_speclist := (make_intargspec p)::!params_speclist;
    params_configlist_doc := (fun () -> (p.doc, (as_string p)))::!params_configlist_doc
  );
  params_configlist := (fun () -> (p.name, (as_string p)))::!params_configlist;
  p

let floatcreate ~name ?(cmdline=false) ?default ~doc ?checker () = 
  let p = register ~name ?default ~doc ~reader:float_of_string ~printer:string_of_float
    ?checker () in
  if cmdline then (
    params_speclist := (make_floatargspec p)::!params_speclist;
    params_configlist_doc := (fun () -> (p.doc, (as_string p)))::!params_configlist_doc
  );
  params_configlist := (fun () -> (p.name, (as_string p)))::!params_configlist;
  p

let boolcreate ~name ?(cmdline=false) ?default ~doc ?checker () = 
  let p = register ~name ?default ~doc ~reader:bool_of_string ~printer:string_of_bool
    ?checker () in
  if cmdline then (
    params_speclist := (make_boolargspec p)::!params_speclist;
    params_configlist_doc := (fun () -> (p.doc, (as_string p)))::!params_configlist_doc
  );
  params_configlist := (fun () -> (p.name, (as_string p)))::!params_configlist;
  p

let stringcreate ~name ?(cmdline=false) ?default ~doc ?checker () = 
  let p = register ~name ?default ~doc ~reader:(fun s -> s) ~printer:(fun s -> s)
    ?checker () in
  if cmdline then (
    params_speclist := (make_stringargspec p)::!params_speclist;
    params_configlist_doc := (fun () -> (p.doc, (as_string p)))::!params_configlist_doc
  );
  params_configlist := (fun () -> (p.name, (as_string p)))::!params_configlist;
  p


let strset param string = (
  let value = try 
    param.reader string 
  with _ -> raise 
    (IllegalParamVal
      (Printf.sprintf "%s is not correct for param %s\n" string param.name))
  in
  set param value
)


let argspeclist () = !params_speclist
    

let configlist () = 
  (* puts only the cmdline-able params in there*)  
  List.sort (fun (a, _) (b, _) -> String.compare a b)
    (List.fold_left 
      (fun configlist f -> 
	try (f())::configlist with
	    NoParamVal _ -> configlist)
      []
      !params_configlist)

let configlist_doc () = 
  (* puts only the cmdline-able params in there*)
  List.sort (fun (a, _) (b, _) -> String.compare a b)
    (List.map (fun f -> f()) !params_configlist_doc)
    
  
let sprintconfig () = 
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
  List.fold_left (^) "" stringlist


let printconfig outchan = 
  let s = sprintconfig() in
  output_string outchan s
