(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2005 Laboratory of Audiovisual Communications (LCAV), and
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


(** Radio transceiver chips. 
  @author Thomas Schmid.
*)

(** Modulation types. *)
type modulation_t = 
  | ASK_noncoherent  (** Non-coherent amplitude-shift keying.*)
  | ASK_coherent     (** Coherent amplitude-shift keying.*)
  | FSK_noncoherent  (** Non-coherent frequency-shift keying.*)
  | FSK_coherent     (** Coherent frequency-shift keying.*)
  | PSK_binary       (** Binary phase-shift keying. *)
  | PSK_differential (** Differential phase-shift keying. *)

(** Phy encoding types. *)
type phy_encoding_t = 
    NRZ       
  | FourbFiveb (** 4b5b *)
  | Manchester
  | Secded

(** Structure describing a radio chip. *)
type t = {
  description : string;
  pt_min : float; (** minimum transmit power in dBm*)
  pt_max : float; (** maximum transmit power in dBm*)
  p_n : float;    (** noise floor in dBm*)
  rate : float;   (** data rate in bps *)
  b_n : float;    (** noise bandwidth in Hz*)
  modulation : modulation_t; (** either noncoherentcask, coherentask, noncoherentfsk/coherentfsk or binarypsk/differentialpsk *)
  encoding : phy_encoding_t; (** one of nrz, 4b5b, manchester, secded *)
}
