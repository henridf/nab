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

(** Channel model. 
  xxx/comment [ overview of what this file provides ]
  @author Thomas Schmid.
*)


(** Parameters representing the RF channel. *)
type channel_parameter_t = {
  description : string;  
  pld0 : float;       (** ref distance power in dB *)
  n : float;          (** path loss exponent *)
  sigma : float;      (** standard deviation of shadowing in dB *)
}
    
val channel_parameters : channel_parameter_t list
  (** A list of channel parameters from "Near Ground Wideband Channel
    Measurements" 
    xxx/comment [ please put complete ref to this book/paper *)
  


val gamma :
  p_t:float ->
  pld0:float ->
  d0:float -> n:float -> sigma:float -> p_n:float -> d:float -> float
  (** Calculates gamma from the given parameters 
    xxx/comment all parameters must be described! (at least one/two words to
    give the name, ie is d 'distance' ? 
  *)
  
val pe : modulation:string -> gamma:float -> b_n:float -> rate:float -> float
  (** Calculates the bit error probability from the given parameters. 
    xxx/comment parameters ;)
*)

val packet_reception_probability :
  encoding:string ->
  framelength:float -> preamblelength:float -> pe:float -> float
  (** Calculates the probability of packet reception for the given
    parameters. 
    xxx/comment are the lengths in bytes or bits?
  *)
