(* very simple parameter handling. incomplete.
   todo :
   - understand unison's one better. 
   - keep 'registry' of names to detect when 2 params with same name are created
   - add ways to save to file a la unison, etc
   - ways to hook better with commandline parsing. ideally, command line
   parsing should fall straight out of the Params created (like parse args
   should compare each token (--nodes/--gui...) with the list of available
   names, if no such token then error, else use Param corresponding to name to
   do the rest)
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
  
