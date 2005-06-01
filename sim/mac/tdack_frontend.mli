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




(** A MAC frontend (see {!Mac_base.frontend}) with modeling of contention, collisions
  and lossy links. It mainly copies the contention mac layer with additional lossy
  links.

  @author Thomas Schmid
*)

type stats = 
    {
      failedRX : int;
      contention_stats : Contention_frontend.stats
    }
      (** Statistics maintained by [mac_tdack] MAC layer 
	 (in addition to statistics from {!Mac.basic_stats}).
	 - [failedRX] counts the number of not received packets because of a too
	 high SNR.
       *)

(** The tdack frontend virtual class. 

  This frontend models contention and collisions similar to the
  {!Contention_frontend.contention_frontend} MAC frontend.

  After a packet is received, the SNR is evaluated after a log-normal distribution
  and it decides if it was capable of decoding or not.
 *)
class virtual tdack_frontend : ?stack:int -> bps:float -> chip:Radiochips.t ->
  #Node.node -> 
object
  inherit Contention_frontend.contention_frontend

    
  method private end_rx : L2pkt.t -> unit
  method private tdack_stats : stats
  method private tdack_reset_stats : unit

  method set_tx_power : float -> unit
end


(** {1 Functions for manipulating {!Contention_frontend} .} *) 

val string_of_ostats : stats -> string
  (** Return a string representation of a {!Contention_frontend}. *)

val add_ostats : stats -> stats -> stats
  (** Add two {!Tdack_frontend}, field by field. *)

val zero_ostats : unit -> stats
  (** Return a {!Tdack_frontend} with all values initialized to 0. *)
