(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Parameter handling. 
  @author Henri Dubois-Ferriere.
*)


(* todo :
   - understand unison's one better. 
   - add ways to save to file a la unison, etc
*)

type 'a t

exception IllegalValue of string

val create : 
  name:string                        (* param name *)
  -> ?shortname:string               (* param shorthand (for cmdline parsing)
					if None, same as name  *)
  -> ?cmdline:bool                   (* True if settable on cmdline. Default
					is false.*)
  -> ?default:'a                     (* if None, can raise exception on a get
					when no value has been set *)
  -> doc:string                      (* documentation string *)
  -> reader:(string -> 'a)           (* convert from string (raise
					IllegalValue if invalid) *)
  -> ?checker:('a -> unit)           (* check validity of value (raise
					IllegalValue if invalid) *)
  -> unit
  -> 'a t                            (* -> new param value *)
  (** Create a parameter of any type *)


val intcreate :  
  name:string  -> ?shortname:string -> ?cmdline:bool
  -> ?default:int  -> doc:string  
  -> ?checker:(int -> unit) -> unit 
  -> int t                             
  (** Create an int parameter *)

val floatcreate :  
  name:string  -> ?shortname:string -> ?cmdline:bool
  -> ?default:float  -> doc:string 
  -> ?checker:(float -> unit) -> unit 
  -> float t                             
  (** Create a float parameter *)

val boolcreate :  
  name:string  -> ?shortname:string -> ?cmdline:bool                      
  -> ?default:bool  -> doc:string  
  -> ?checker:(bool -> unit) -> unit 
  -> bool t                             
  (** Create a bool parameter *)


val stringcreate :  
  name:string  -> ?shortname:string -> ?cmdline:bool  
  -> ?default:string  -> doc:string 
  -> ?checker:(string -> unit) -> unit 
  -> string t                             
  (** Create a string parameter *)



val set : 'a t -> 'a -> unit
val strset : 'a t -> string -> unit
val get : 'a t -> 'a                 (* fails if default was
					None and value was not set *)
  
val make_argspeclist : unit -> (string * Myarg.spec * string) list
  (** Returns a list of triples [(key, spec, doc)] corresponding to all
    created params which had 'cmdline' set. This list can then be used 
    to call Arg.parse with. *)
 
val dumpconfig : unit -> (string * string) list
  (** Returns a list of (keyword, value) pairs corresponding to the values of
    all created params which had cmdline set.*)


val make_cmdline_able : 'a t -> unit
val make_not_cmdline_able : 'a t -> unit
