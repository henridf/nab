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



(** Parameter handling. 
  The external was partly inspired by unison's Prefs module.
  @author Henri Dubois-Ferriere.
*)


type 'a t
  (** The type of [Param] parameters. *)

exception IllegalParamVal of string


(** {1 Getting and Setting parameter values} *)

val set : 'a t -> 'a -> unit
  (** Set the value of a parameter. 
    @raise IllegalParamVal if the parameter provided is invalid ('validity' is a
    parameter-specific notion. *)

val strset : 'a t -> string -> unit
  (** Set the value of a parameter, by providing a string representation of
    the value.
    @raise IllegalParamVal if the string provided cannot be parsed into a
    param value. *)

val get : 'a t -> 'a                 
  (** Returns the value of this parameter. *)



(** {1 Creating and registering parameters} *)


val create : 
  name:string ->             
  ?shortname:string ->       
  ?default:'a ->             
  doc:string ->              
  reader:(string -> 'a) ->   
  ?checker:('a -> unit) ->   
  unit 
  -> 'a t                    
(** 
  Create a parameter, of any type ['a].
  @param name The name of the parameter.
  @param shortname Shorter name, used for commandline parsing. Optional; if
  absent command-line parsing is not possible for this parameter.
  @param default Default value. Optional.
  @param doc A textual description of the parameter.
  @param reader A function which converts a string to the parameter (used by [Param.strset]), should raise any exception if parsing fails.
  @param checker A function to check validity of value when setting, for
  example to check that the number of nodes is positive. Optional.
*)

val intcreate :  
  name:string  -> 
  ?shortname:string -> 
  ?cmdline:bool ->    (* True if settable on cmdline. Default  is false.*)
  ?default:int  -> 
  doc:string  -> 
  ?checker:(int -> unit) -> 
  unit 
  -> int t                             
  (** Create an parameter of type int.
    @param cmdline Allow the parameter to be set via a command-line
    argument. Optional; default is [false].
    Other arguments are same as for {!Param.create}. *)

val floatcreate :  
  name:string  -> 
  ?shortname:string -> 
  ?cmdline:bool ->  (* True if settable on cmdline. Default  is false.*)
  ?default:float  -> 
  doc:string -> 
  ?checker:(float -> unit) -> unit 
  -> float t                             
  (** Create an parameter of type float.
    @param cmdline Allow the parameter to be set via a command-line
    argument. Optional; default is [false].
    Other arguments are same as for {!Param.create}. *)


val boolcreate :  
  name:string  -> 
  ?shortname:string -> 
  ?cmdline:bool  ->  (* True if settable on cmdline. Default  is false.*)
  ?default:bool  -> 
  doc:string  
  -> ?checker:(bool -> unit) -> unit 
  -> bool t                             
  (** Create an parameter of type bool.
    @param cmdline Allow the parameter to be set via a command-line
    argument. Optional; default is [false].
    Other arguments are same as for {!Param.create}. *)


val stringcreate :  
  name:string  -> 
  ?shortname:string -> 
  ?cmdline:bool  -> (* True if settable on cmdline. Default  is false.*)
  ?default:string  -> 
  doc:string -> 
  ?checker:(string -> unit) -> unit 
  -> string t                             
  (** Create an parameter of type string.
    @param cmdline Allow the parameter to be via a command-line argument. Optional; default is [false].
    Other arguments are same as for {!Param.create}. *)




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
