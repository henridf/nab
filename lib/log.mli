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







(** Logging facilities. 
  @author Henri Dubois-Ferriere.
*)

val ochan : out_channel ref
  (** The channel onto which logs are written. 
    By default, stderr.*)

(** Logging levels, in order of increasing 'importance'. *)
type log_level_t =
  | LOG_DEBUG
  | LOG_INFO
  | LOG_NOTICE
  | LOG_WARNING
  | LOG_ERROR
  | LOG_ALWAYS
      

val set_log_level : level:log_level_t -> unit

val strset_log_level : string -> unit
  (** Set the log level via a string (for example provided as cmdline argument). *)

(** This class provides a bunch of straightforward logging methods, and is
  designed to be inherited by any class which needs to log messages. 

  [log_*] methods are private so that only the object itself can use them, and
  so that inheriting classes do not need to explicitly list all these method
  in the interface *)
class inheritable_loggable :

  object
    val mutable objdescr : string
      (** This is the description string that is put in second place (after
	the timestamp). AS for example the "/node/3/cmac" in the following line:
	
	0.468795 /node/3/cmac TX packet 
      *)

    method private log_always : msg:string Lazy.t -> unit
    method private log_debug : msg:string Lazy.t -> unit
    method private log_error : msg:string Lazy.t -> unit
    method private log_info : msg:string Lazy.t -> unit
    method private log_notice : msg:string Lazy.t -> unit
    method private log_warning : msg:string Lazy.t -> unit

    method private log : string Lazy.t -> unit
      (** Shorthand for {!Log.inheritable_loggable.log_info} *)

    method private mark_break : unit
      (** Inserts a breaking line (a line of '-') into the logging steam. Use
	(sparingly) to visually separate portions of the log. *)

    method objdescr : string
      (** Returns this object's objdescr *)

    method private set_objdescr : ?owner:inheritable_loggable -> string -> unit
      (** This should be used by an inheriting class to set the objdescr. If
	the object is "owned" by another object, it can pass along the owner
	so that the objdescr will be built as the concatenation of the owner's
	objdescr and the ownee's objdescr. 
	For example, a mac object belonging to node 3 may simply set its
	objdescr as "/cmac", and the resulting objdescr will be
	"/node/3/cmac".*)

  end

(** This class is equivalent to {!Log.inheritable_loggable} except that its
  logging methods are public. It is intended to be used outside of objects, by
  simply instanciating a new object of this class. 
  The constructor takes the objdescr as parameter. *)
class standalone_loggable : string ->
  object
    val mutable objdescr : string
    method log : string Lazy.t -> unit
    method log_always : msg:string Lazy.t -> unit
    method log_debug : msg:string Lazy.t -> unit
    method log_error : msg:string Lazy.t -> unit
    method log_info : msg:string Lazy.t -> unit
    method log_notice : msg:string Lazy.t -> unit
    method log_warning : msg:string Lazy.t -> unit
    method mark_break : unit
    method objdescr : string
  end

(** A globally available {!Log.standalone_loggable} object, which may be used from anywhere (e.g.,
  scripts, setup code) where it is not appropriate to instanciate a new
  {!Log.standalone_loggable} object. *)
val log : standalone_loggable


(** This class is equivalent to  {!Log.standalone_loggable} except that it
  logs messages without a time field *)
class standalone_loggable_notime :
  string ->
  object
    val mutable objdescr : string
    method log : string Lazy.t -> unit
    method log_always : msg:string Lazy.t -> unit
    method log_debug : msg:string Lazy.t -> unit
    method log_error : msg:string Lazy.t -> unit
    method log_info : msg:string Lazy.t -> unit
    method log_notice : msg:string Lazy.t -> unit
    method log_warning : msg:string Lazy.t -> unit
    method mark_break : unit
    method objdescr : string
  end

(** A globally available {!Log.standalone_loggable_notime} object, which may be used from anywhere (e.g.,
  scripts, setup code) where it is not appropriate to instanciate a new
  {!Log.standalone_loggable_notime} object. *)
val lognt : standalone_loggable_notime

