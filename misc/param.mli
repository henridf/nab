(* very simple parameter handling. incomplete.
   todo :
   - understand unison's one better. 
   - keep 'registry' of names to detect when 2 params with same name are created
   - add ways to save to file a la unison, etc
   - ways to hook better with commandline parsing. ideally, command line
   parsing should fall straight out of the Params created. Maybe params can
   automatically generate a Arg.spec list, and then use that to parse cmdline opts??
*)

type 'a t

exception IllegalValue of string

val create : 
     name:string                     (* param name *)
  -> default:'a option               (* if None, can raise exception on a get
					when no value has been set *)
  -> doc:string                      (* documentation string *)
  -> reader:(string -> 'a)           (* convert from string (raise
					IllegalValue if invalid) *)
  -> checker:('a -> unit) option     (* check validity of value (raise
					IllegalValue if invalid) *)
  -> 'a t                            (* -> new param value *)

val set : 'a t -> 'a -> unit
val strset : 'a t -> string -> unit
val get : 'a t -> 'a                 (* fails if default was
					None and value was not set *)
  
