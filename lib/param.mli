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
exception NoParamVal of string

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

val as_string : 'a t -> string


(** {1 Creating and registering parameters} *)


val create : 
  name:string ->             
  doc:string ->              
  reader:(string -> 'a) ->   
  printer:('a -> string) ->
  ?cmdline:bool ->    (* True if settable on cmdline. Default  is false.*)
  ?default:'a ->             
  ?checker:('a -> unit) ->   
  unit 
  -> 'a t                    
(** 
  Create a parameter, of any type ['a].
  @param name The name of the parameter.
  @param doc A textual description of the parameter.
  @param reader A function which converts a string to the parameter (used by
  @param printer A function which returns a string representing the value of
  the parameter.
  @param default Default value. Optional.
  @param cmdline Allow the parameter to be set via a command-line
  argument. Optional; default is [false].
*)

val intcreate :  
  name:string  -> 
  ?cmdline:bool ->    (* True if settable on cmdline. Default  is false.*)
  ?default:int  -> 
  doc:string  -> 
  ?checker:(int -> unit) -> 
  unit 
  -> int t                             
  (** Create an parameter of type int.
    @param checker A function to check validity of value when setting, for
    example to check that the number of nodes is positive. Optional.
    Other arguments are same as for {!Param.create}. *)

val floatcreate :  
  name:string  -> 
  ?cmdline:bool ->  (* True if settable on cmdline. Default  is false.*)
  ?default:float  -> 
  doc:string -> 
  ?checker:(float -> unit) -> unit 
  -> float t                             
  (** Create an parameter of type float.
    @param checker A function to check validity of value when setting, for
    example to check that the number of nodes is positive. Optional.
    Other arguments are same as for {!Param.create}. *)


val boolcreate :  
  name:string  -> 
  ?cmdline:bool  ->  (* True if settable on cmdline. Default  is false.*)
  ?default:bool  -> 
  doc:string  
  -> ?checker:(bool -> unit) -> unit 
  -> bool t                             
  (** Create an parameter of type bool.
    @param checker A function to check validity of value when setting, for
    example to check that the number of nodes is positive. Optional.
    Other arguments are same as for {!Param.create}. *)


val stringcreate :  
  name:string  -> 
  ?cmdline:bool  -> (* True if settable on cmdline. Default  is false.*)
  ?default:string  -> 
  doc:string -> 
  ?checker:(string -> unit) -> unit 
  -> string t                             
  (** Create an parameter of type string.
    @param checker A function to check validity of value when setting, for
    example to check that the number of nodes is positive. Optional.
    Other arguments are same as for {!Param.create}. *)


val argspeclist : unit -> (string * Arg.spec * string) list
  (** Returns a list of triples [(key, spec, doc)] corresponding to all
    created params which had 'cmdline' set. This list can then be used 
    to call the standard library's Arg.parse with, in order to set Param
    values from the command line. *)

val configlist : unit -> (string * string) list
  (** Returns a list of (keyword, value) pairs containing all created params
    which have a value (ie, those params which were created without a default
    value and have not been set are not included).*)

val sprintconfig : unit -> string
  (** Returns a string representation of all registered Param (not only those
    from params.ml) and their values.
    Right now only those params that are command-line settable are dumped (ie
    created with ~cmdline=true).*)
  (* maybe need an option to require *all* parameters to be returned *)

val printconfig : out_channel -> unit
  (** Prints a string representation of all registered Param (not only those
    from params.ml) and their values.
    Right now only those params that are command-line settable are dumped (ie
    created with ~cmdline=true).*)
(* maybe need an option to require *all* parameters to be returned *)




