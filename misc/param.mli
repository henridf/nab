(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(** Parameter handling. *)


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
					IllegalValue if invalid) *)
  -> unit
  -> 'a t                            (* -> new param value *)
  (** Create a parameter of any type *)


val intcreate :  
  name:string  -> ?shortname:string 
  -> ?default:int  -> doc:string                      
  -> ?checker:(int -> unit) -> unit 
  -> int t                             
  (** Create an int parameter *)

val floatcreate :  
  name:string  -> ?shortname:string 
  -> ?default:float  -> doc:string                      
  -> ?checker:(float -> unit) -> unit 
  -> float t                             
  (** Create a float parameter *)

val set : 'a t -> 'a -> unit
val strset : 'a t -> string -> unit
val get : 'a t -> 'a                 (* fails if default was
					None and value was not set *)
  


val make_intargspec : int t -> (string * Arg.spec * string) 
  (** Returns a triple [(key, spec, doc)] which can be used 
    to call Arg.parse with. *)

val make_floatargspec : float t -> (string * Arg.spec * string) 
  (** Returns a triple [(key, spec, doc)] which can be used 
    to call Arg.parse with. *)

