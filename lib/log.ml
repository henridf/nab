(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Header *)







open Printf

let ochan = ref Pervasives.stderr


type log_level_t = LOG_DEBUG | LOG_INFO | LOG_NOTICE | LOG_WARNING | LOG_ERROR
		   | LOG_ALWAYS

let string_of_loglevel l = 
  match l with 
    | LOG_DEBUG -> "debug"
    | LOG_INFO -> "info"
    | LOG_NOTICE -> "notice"
    | LOG_WARNING -> "warning"
    | LOG_ERROR -> "error"
    | LOG_ALWAYS -> "always"
		       
let loglevel_of_string s = 
  match s with 
    | "debug" -> LOG_DEBUG 
    | "info" -> LOG_INFO 
    | "notice" -> LOG_NOTICE
    | "warning" | "warn" -> LOG_WARNING
    | "error" -> LOG_ERROR 
    | "always" -> LOG_ALWAYS
    | _ -> raise (Failure "Invalid format for loglevel")
		       
(* can't find anything 'official' about ocaml guaranteeing the ordering, so
   let's check to be positive *)
let _ = 
  ((LOG_DEBUG < LOG_INFO 
  &&
  LOG_INFO < LOG_NOTICE
  &&
  LOG_NOTICE < LOG_WARNING
  &&
  LOG_WARNING < LOG_ERROR 
  &&
  LOG_ERROR < LOG_ALWAYS)
  || 
  raise (Failure "Unexpected ordering on enumerated type Log.log_level_t"))
  
let current_log_level = ref LOG_WARNING
let set_log_level ~level:l = current_log_level := l

let strset_log_level s = current_log_level := loglevel_of_string s


(* Logging-related behavior that classes can inherit from.
   s#objdescr should be set in the initializer of the inheritor.

   There are two types of logging methods:
   - logmsg* methods take naml-typed messages. This allows more
     efficiency (and less code clutter) when a message is being 
     constructed for Naml needs to get logged as well.

   - log* methods take strings, and are intended to be used in 
     situations where no corresponding naml message is being emitted.

 *)

class inheritable_loggable = 
object(s) 
  val mutable objdescr = ""
  method private set_objdescr 
    ?(owner : inheritable_loggable option)
    s = 
    let base = 
      match owner with 
	| None -> ""
	| Some o -> 
	    if o#objdescr.[String.length o#objdescr -1] = ' ' 
	    then String.sub o#objdescr 0 (String.length o#objdescr -1)
	    else o#objdescr
    in
    objdescr <- base ^ s ^ " " (* add space here so we don't have to do it
				 each time we write a log *)

  method objdescr = objdescr

  method private log_level ~msg:msg ~level:l =
    if l >= !current_log_level then (
      output_string !ochan (sprintf "%f " (Time.get_time()));
      output_string !ochan objdescr;
      output_string !ochan (Lazy.force msg);
      output_char !ochan '\n';
      flush !ochan;
    )

  method private log msg = s#log_level ~level:LOG_INFO ~msg:msg

  (* shorthand wrappers for log_level *)
  method private log_debug = s#log_level ~level:LOG_DEBUG
  method private log_info = s#log_level ~level:LOG_INFO 
  method private log_notice = s#log_level ~level:LOG_NOTICE
  method private log_warning = s#log_level ~level:LOG_WARNING
  method private log_error = s#log_level ~level:LOG_ERROR
  method private log_always = s#log_level ~level:LOG_ALWAYS
  method private mark_break = (
      output_string !ochan
	"\n-------------------------------------------------------\n\n";
    flush !ochan
  )

end 

class standalone_loggable facility = 
object
  inherit inheritable_loggable as super
  initializer (objdescr <- facility)

  (* This notation turns the inherited private methods into public methods.
     Another (possibly clearer) syntax would have been to write
     method log = super#log
  *)
  method virtual log : _
  method virtual log_debug : _
  method virtual log_info : _
  method virtual log_notice : _
  method virtual log_warning : _
  method virtual log_error : _
  method virtual log_always : _
  method virtual mark_break : _

end

class standalone_loggable_notime facility = 
object 
  inherit standalone_loggable facility as super
  initializer (objdescr <- facility)

  method private log_level ~msg:msg ~level:l =
    if l >= !current_log_level then (
      output_string !ochan objdescr;
      output_string !ochan (Lazy.force msg);
      output_char !ochan '\n';
      flush !ochan;
    )

end

let log = (new standalone_loggable "/global ")
let lognt = (new standalone_loggable_notime "/global ")
