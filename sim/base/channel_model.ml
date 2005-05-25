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
open Radiochips



type channel_parameter_t =
  {
    pld0 : float;         (* ref distance power in dB *)
    n : float;            (* path loss exponent *)
    sigma : float         (* standard deviation of shadowing in dB *)
  }

type rf_environment = 
 | Engineering_1    
 | Appt_hallway     
 | Parking_struct   
 | One_side_corr    
 | One_side_pat     
 | Concrete_canyon  
 | Plant_fence      
 | Small_boulders   
 | Beach            
 | Bamboo           
 | Underbrush       
 | Synthetic             
		    

let channel_params rf = match rf with 
  | Engineering_1 ->      
      { 
	pld0 = -44.75;
	n = 1.9;
	sigma = sqrt 5.7
      } 

  | Appt_hallway ->  
      {
	pld0 = -36.6;
	n = 2.0;
	sigma = sqrt 8.0
      }

  | Parking_struct -> 
      {
	pld0 = -34.4;
	n = 3.0;
	sigma = sqrt 7.9
      }

  | One_side_corr -> 
      {
	pld0 = -38.85;
	n = 1.9;
	sigma = sqrt 8.0
      }

  | One_side_pat -> 
      {
	pld0 = -36.6;
	n = 3.2;
	sigma = sqrt 3.7
      }

  | Concrete_canyon -> {
      pld0 = -46.35;
      n = 2.7;
      sigma = sqrt 10.2
    }

  | Plant_fence -> {
      pld0 = -36.35;
      n = 4.9;
      sigma = sqrt 9.4
    }

  | Small_boulders -> {
      pld0 = -39.35;
      n = 3.5;
      sigma = sqrt 12.8
    }

  | Beach -> {
      pld0 = -39.15;
      n = 4.2;
      sigma = sqrt 4.0
    }

  | Bamboo -> {
      pld0 = -36.7;
      n = 5.0;
      sigma = sqrt 11.6
    }

  | Underbrush -> {
      pld0 = -34.8;
      n = 3.6;
      sigma = sqrt 8.4
    }

  | Synthetic -> {
      pld0 = 40.;
      n = 4.;
      sigma = 4.
    } 




let rng = Gsl_rng.make (Gsl_rng.default ())

let gamma ~rf ~p_t ~d0 ~p_n ~d =
  let chan = channel_params rf in
  let n0s = Gsl_randist.gaussian rng ~sigma:chan.sigma in
  10. ** ((p_t -. chan.pld0 -. 10. *. chan.n *. log10 (d /. d0) -. p_n -. n0s ) /. 10.)

let pe ~modulation ~gamma ~b_n ~rate =
  let gbnr = gamma *. b_n /. rate in
  match modulation with
      ASK_noncoherent -> 1. /. 2. *. (exp (-.gbnr /. 2.) +. Misc.qfunct (sqrt gbnr))
    | ASK_coherent  -> Misc.qfunct (sqrt (gbnr /. 2.))
    | FSK_noncoherent -> 1. /. 2. *. exp (-.gbnr /. 2.)
    | FSK_coherent -> Misc.qfunct ( sqrt gbnr )
    | PSK_binary -> Misc.qfunct ( sqrt (2. *. gbnr))
    | PSK_differential -> 1. /. 2. *. exp (-.gbnr) 


(** framelength and preamblelength are in byte! *)
let packet_reception_probability ~encoding ~framelength ~preamblelength ~pe =
  let pe' = 1. -. pe in
  let l = preamblelength in
  let f = framelength in
  match encoding with
      NRZ -> pe' ** (8. *. l) *. pe' ** (8. *. (f -. l))
    | FourbFiveb ->  pe' ** (8. *. l) *. pe' ** (8. *. (f -. l) *. 1.25)
    | Manchester ->  pe' ** (8. *. l) *. pe' ** (8. *. (f -. l) *. 2.0)
    | Secded -> pe' ** (8. *. l) *. (pe' ** 8. +. 8. *. pe *. pe' ** 7.) ** ((f -. l) *. 3.0)

    
(*
let cc1000_rec_proba ?(scenario=0) f l d =
  let channel = List.nth channel_parameters scenario in
  let chip = List.nth radio_chips 0 in
  let d0 = 1. in
  let pt = chip.pt_max in

  let g = gamma pt channel.pld0 d0 channel.n channel.sigma chip.p_n d in
    Printf.printf "gamma=%f\n" g;
  let error_probability = pe chip.modulation g chip.b_n chip.rate in
    
    packet_reception_probability chip.encoding f l error_probability

*)
