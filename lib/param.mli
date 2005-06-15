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
  Some of this was initially inspired by unison's Prefs module.
  @author Henri Dubois-Ferriere.
*)


type 'a t
  (** The type of [Param] parameters. *)

exception IllegalParamVal of string
exception NoParamVal of string

(** {1 Creating and registering parameters} *)


val create : 
  name:string ->             
  doc:string ->              
  reader:(string -> 'a) ->   
  printer:('a -> string) ->
  ?cmdline:bool ->    (* True if settable on cmdline. Default  is false.*)
  ?default:'a ->             
  ?checker:('a -> unit) ->   
  ?notpersist:bool ->
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
  @param checker A function to check validity of value when setting, for
  example to check that the number of nodes is positive. Optional.
  @param persist If true, ignore this parameter when saving and restoring configuration
  state. Default is [false].

*)

val intcreate :  
  name:string  -> 
  doc:string  -> 
  ?cmdline:bool ->    
  ?default:int  -> 
  ?checker:(int -> unit) -> 
  ?notpersist:bool ->
  unit 
  -> int t                             
  (** Create a parameter of type int.
    Arguments are same as for {!Param.create}. *)

val floatcreate :  
  name:string  -> 
  doc:string -> 
  ?cmdline:bool ->  
  ?default:float  -> 
  ?checker:(float -> unit) -> 
  ?notpersist:bool ->
  unit 
  -> float t                             
  (** Create a parameter of type float.
    Arguments are same as for {!Param.create}. *)


val boolcreate :  
  name:string  -> 
  doc:string  ->
  ?cmdline:bool  ->  
  ?default:bool  -> 
  ?notpersist:bool ->
  unit 
  -> bool t                             
  (** Create a parameter of type bool.
    Arguments are same as for {!Param.create}. *)


val stringcreate :  
  name:string  -> 
  doc:string -> 
  ?cmdline:bool  -> 
  ?default:string  -> 
  ?checker:(string -> unit) -> 
  ?notpersist:bool ->
  unit 
  -> string t                             
  (** Create a parameter of type string.
    Arguments are same as for {!Param.create}. *)


(** {1 Getting and Setting parameter values} *)

val set : 'a t -> 'a -> unit
  (** Set the value of a parameter. 
    @raise IllegalParamVal if the parameter provided is invalid ('validity' is a
    parameter-specific notion. *)

val strset : 'a t -> string -> unit
  (** [strset param stringval] sets the value of parameter [param] to the
    value represented by [stringval], converting it to the appropriate type if
    [param] is not a string parameter.
    @raise IllegalParamVal if the string provided cannot be parsed into a
    param value. *)

val strset_by_name : string -> string -> unit
  (** [strset paramname stringval] sets the value of the parameter with name
    [paramname] to the value represented by [stringval], converting it to the
    appropriate type if it is not a string parameter.
    @raise IllegalParamVal if the string provided cannot be parsed into a
    param value. *)

val get : 'a t -> 'a                 
  (** Returns the value of this parameter.
    @raise NoParamVal if the parameter's value was not set and it has not default.
  *)

val as_string : 'a t -> string
  (** Returns a string representation of this parameter. *)

val has_value : 'a t -> bool
  (** [has_value param] returns true if the value of param has been set, false
    otherwise. *)
    

(** {1 Operations on all configured parameters at a time}*)

val argspeclist : unit -> (string * Arg.spec * string) list
  (** Returns a list of triples [(key, spec, doc)] corresponding to all
    created params which had 'cmdline' set. This list can then be used 
    to call the standard library's Arg.parse with, in order to set Param
    values from the command line. *)

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



module Persist : Persist.t
