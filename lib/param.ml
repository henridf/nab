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
  (* contains all params that have cmdline=true.
     used for parsing command line arguments.
  *)

let params_configlist : (unit -> string * string) list ref = ref []
  (* for all params except those with notpersist=true, contains a function
     which returns the name field and the value (as_string) of the param. 
     Used to save and restore configuration state.
  *)

let params_configlist_doc : (unit -> string * string) list ref = ref []
  (* for cmdline params only, contains a function which returns the doc field
     and the  value (as_string) of the param. 
     Used to print out configuration state.
  *)

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

let strset param string = (
  let value = try 
    param.reader string 
  with _ -> raise 
    (IllegalParamVal
      (Printf.sprintf "%s is not correct for param %s" string param.name))
  in
  set param value
)

let strset_by_name name value = 
  try 
    let f = Hashtbl.find namespace name in
    f value
  with Not_found -> failwith ("No registered param with name "^name)

let register ~cmdline ~name ?default ~doc ~reader ~printer ?checker
  ?(notpersist=false) () =  

  if Hashtbl.mem namespace name then 
    raise (Failure (Printf.sprintf "Param.register : %s already taken" name));
  
  begin
    match checker, default with 
      | Some f, Some v -> f v
      | _ -> ();
  end;
  let p = {
    value=default;
    name=name;
    doc=doc;
    reader=reader;
    printer=printer;
    checker=checker} in

  Hashtbl.add namespace name (fun s -> strset p s);

  if not notpersist then
    params_configlist := 
    (fun () -> (p.name, (as_string p)))::!params_configlist;

  if cmdline then 
    params_configlist_doc := 
    (fun () -> (p.doc, (as_string p)))::!params_configlist_doc;
  
  p
    


let create ~name ~doc ~reader ~printer ?(cmdline=false) ?default ?checker
  ?(notpersist=false) () = 

  let p = register ~cmdline ~name ~printer ?default ~doc ~reader ?checker
    ~notpersist () in
  if cmdline then 
    params_speclist := (make_otherargspec p)::!params_speclist;
  p

    
let intcreate ~name  ~doc ?(cmdline=false) ?default ?checker
  ?(notpersist=false) () = 
  let p = register ~cmdline ~name ?default ~doc ~reader:int_of_string
    ~printer:string_of_int ~notpersist ?checker () in
  if cmdline then 
    params_speclist := (make_intargspec p)::!params_speclist;
  p

let floatcreate ~name  ~doc ?(cmdline=false) ?default ?checker
  ?(notpersist=false) () = 
  let p = register ~cmdline ~name ?default ~doc ~reader:float_of_string
    ~printer:string_of_float ?checker ~notpersist () in
  if cmdline then 
    params_speclist := (make_floatargspec p)::!params_speclist;
  p

let boolcreate ~name  ~doc ?(cmdline=false) ?default ?checker
  ?(notpersist=false) () = 
  let p = register ~cmdline ~name ?default ~doc ~reader:bool_of_string
    ~printer:string_of_bool ?checker ~notpersist () in
  if cmdline then 
    params_speclist := (make_boolargspec p)::!params_speclist;
  p

let stringcreate ~name  ~doc ?(cmdline=false) ?default ?checker
  ?(notpersist=false) () = 
  let p = register ~cmdline ~name ?default ~doc ~reader:(fun s -> s)
    ~printer:(fun s -> s) ?checker ~notpersist () in
  if cmdline then 
    params_speclist := (make_stringargspec p)::!params_speclist;
  p


let argspeclist () = !params_speclist
    

(** Returns a list of (keyword, value) pairs containing all created params
  which have a value (ie, those params which were created without a default
  value and have not been set are not included).*)
let configlist () = 
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
  let l = configlist_doc () in
  
  let max_width = 
    List.fold_left 
      (fun w (name,_) -> 
	if String.length name > w then String.length name else w)
      0
      l
  in
  
  let stringlist = 
    List.map (fun (name, value) ->
      (Misc.padto name (max_width + 4))^value^"\n"
    ) l
  in
  List.fold_left (^) "" stringlist


let printconfig outchan = 
  let s = sprintconfig() in
  output_string outchan s


module Persist = struct

  let save oc = Marshal.to_channel oc (configlist()) []
    
  let restore ?(verbose=true) ic = 
    Log.log#log_notice (lazy "Restoring Param state...");
    let configlist = (Marshal.from_channel ic  : (string * string) list)
    in
    List.iter (fun (name, value) -> strset_by_name name value) configlist;
    if verbose then 
      Log.log#log_notice (lazy (sprintconfig()));
    Log.log#log_notice (lazy "Done (restoring param state).");
end
