



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
  -> ?default:'a                     (* if None, can raise exception on a get
					when no value has been set *)
  -> doc:string                      (* documentation string *)
  -> reader:(string -> 'a)           (* convert from string (raise
					IllegalValue if invalid) *)
  -> ?checker:('a -> unit)           (* check validity of value (raise
					IllegalValue if invalid). If this has
					side-effects, should be idempotent. *)
  -> unit
  -> 'a t                            (* -> new param value *)
  (** Create a parameter of any type *)


val intcreate :  
  name:string  -> 
  ?shortname:string -> 
  ?cmdline:bool ->    (* True if settable on cmdline. Default  is false.*)
  ?default:int  -> 
  doc:string  -> 
  ?checker:(int -> unit) -> 
  unit 
  -> int t                             
  (** Create an int parameter. Arguments are same as for {!Param.create}. *)

val floatcreate :  
  name:string  -> 
  ?shortname:string -> 
  ?cmdline:bool ->  (* True if settable on cmdline. Default  is false.*)
  ?default:float  -> 
  doc:string -> 
  ?checker:(float -> unit) -> unit 
  -> float t                             
  (** Create a float parameter. Arguments are same as for {!Param.create}. *)

val boolcreate :  
  name:string  -> 
  ?shortname:string -> 
  ?cmdline:bool  ->  (* True if settable on cmdline. Default  is false.*)
  ?default:bool  -> 
  doc:string  
  -> ?checker:(bool -> unit) -> unit 
  -> bool t                             
  (** Create a bool parameter. Arguments are same as for {!Param.create}. *)


val stringcreate :  
  name:string  -> 
  ?shortname:string -> 
  ?cmdline:bool  -> (* True if settable on cmdline. Default  is false.*)
  ?default:string  -> 
  doc:string -> 
  ?checker:(string -> unit) -> unit 
  -> string t                             
  (** Create a string parameter. Arguments are same as for {!Param.create}. *)



val set : 'a t -> 'a -> unit
val strset : 'a t -> string -> unit
val get : 'a t -> 'a                 (* fails if default was
					None and value was not set *)
  
val make_argspeclist : unit -> (string * Arg.spec * string) list
  (** Returns a list of triples [(key, spec, doc)] corresponding to all
    created params which had 'cmdline' set. This list can then be used 
    to call Arg.parse with. *)
 

val configlist : unit -> (string * string) list
  (** Returns a list of (keyword, value) pairs corresponding to the values of
    all created params which had cmdline set.*)


val sprintconfig : unit -> string
  (** Returns a string representation of all registered Param (not only those
    from params.ml) and their values.
    Right now only those params that are command-line settable are dumped (ie
    created with ~cmdline=true).*)

val printconfig : out_channel -> unit
  (** Prints a string representation of all registered Param (not only those
    from params.ml) and their values.
    Right now only those params that are command-line settable are dumped (ie
    created with ~cmdline=true).*)

(*
  val make_cmdline_able : 'a t -> unit
  val make_not_cmdline_able : 'a t -> unit
*)
