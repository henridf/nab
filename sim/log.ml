(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* Logging facilities. This is for spitting out messages about the
   progress/status/errors of the ongoing simulation *)

open Printf

let output_fd = ref Pervasives.stderr


type log_level_t = LOG_DEBUG | LOG_INFO | LOG_NOTICE | LOG_WARNING | LOG_ERROR
		   | LOG_ALWAYS

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
  
let current_log_level = ref LOG_NOTICE
let set_log_level ~level:l = current_log_level := l


(* Logging-related behavior that classes can inherit from.
   s#objdescr should be set in the initializer of the inheritor.

   There are two types of logging methods:
   - logmsg* methods take naml-typed messages. This allows more
     efficiency (and less code clutter) when a message is being 
     constructed for Naml needs to get logged as well.

   - log* methods take strings, and are intended to be used in 
     situations where no corresponding naml message is being emitted.

 *)


class loggable = 
object(s) 
  val mutable objdescr = ""
  method private set_objdescr s = objdescr <- s ^ " " (* add space here so we
							 don't have to do it
							 each time we write a
							 log *)
  method objdescr = objdescr

  method private log_level ~msg:msg ~level:l =
    if l >= !current_log_level then (
      output_string !output_fd (sprintf "%f " (Common.get_time()));
      output_string !output_fd objdescr;
      output_string !output_fd (Lazy.force msg);
      output_char !output_fd '\n';
      flush !output_fd;
    )

  method private log msg = s#log_level ~level:LOG_INFO ~msg:msg

    (* shorthand wrappers for log_level *)
  method private log_debug = s#log_level ~level:LOG_DEBUG
  method private log_info = s#log_level ~level:LOG_INFO 
  method private log_notice = s#log_level ~level:LOG_NOTICE
  method private log_warning = s#log_level ~level:LOG_WARNING
  method private log_error = s#log_level ~level:LOG_ERROR
  method private log_always = s#log_level ~level:LOG_ALWAYS





end 

class global_loggable = 
  (* a globally accessible log instance, to be used by people who 
     don't inherit from log (like in scripts) *)
object
  inherit loggable as super
  method set_objdescr descr = objdescr <- descr
  method log = super#log
  method log_debug = super#log_debug
  method log_info = super#log_info
  method log_notice = super#log_notice
  method log_warning = super#log_warning
  method log_error = super#log_error
  method log_always = super#log_always
end

let log = (new global_loggable)
let () = log#set_objdescr "/global"
