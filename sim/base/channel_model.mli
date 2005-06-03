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

    This file provides a shadowing channel model and different radio channel
    measurements. The gamma function provides a simple way to calculate the
    SNR at the receiver. pe calculates
    the bit error probability and error_probability the probability that a whole
    message is received correctly at the receiver, given a certain encoding.

  @author Thomas Schmid.
*)


(** RF channel parameters from "Near Ground Wideband Channel
  Measurements", K. Sohrabi, B. Manriquez, G. Pottie.
*)
type rf_environment = 
 | Engineering_1    (** Engineering I *)
 | Appt_hallway     (** Appartment Hallway *)
 | Parking_struct   (** Parking Structure *)
 | One_side_corr    (** One-sided Corridor *)
 | One_side_pat     (** One-sided Patio *)
 | Concrete_canyon  (** Concrete Canyon *)
 | Plant_fence      (** Plant Fence *)
 | Small_boulders   (** Small Boulders *)
 | Beach            (** Sandy Flat Beach *)
 | Bamboo           (** Dense Bamboo *)
 | Underbrush       (** Dry Tall Underbrush *)
 | Synthetic        (** my own, pld0 only an estimate. with -10dBm rx power we
		      get approx 10-15m range *)


val gamma :  rf:rf_environment ->  p_t:float -> d0:float -> p_n:float -> 
  d:float -> float
  (** Calculates gamma (received SNR and receiver) in [dB] from the given parameters 

    [rf]: Type of channel model to use (see {!Channel_mode.rf_environment}).
    [p_t]: Transmit power
    [d0]: Distance at which PL(d0), i.e. the path loss at distance d0, has been 
    calculated/ measured
    [p_n]: Noise floor of the used radio chip
    [d]: Distance between the sender and receiver in meters
  *)
  
val pe : modulation:Radiochips.modulation_t -> gamma:float -> b_n:float -> rate:float -> float
  (** Calculates the bit error probability from the given parameters. 
      [gamma]: Receiver SNR in dB
      [b_n]: Noise bandwith
      [rate]: Data rate at which one is communicating
*)

val packet_reception_probability :
  encoding:Radiochips.phy_encoding_t ->
  framelength:int -> preamblelength:int -> pe:float -> float
  (** Calculates the probability of packet reception for the given
    parameters.
    [encoding]: Used encoding in the radio chip
    [framelength]: length of framein byte
    [preamblelength]: length of preamble (in bytes)
    [pe]: bit error probability
  *)
