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







let initial_hash_size = 1000

type enc_t = {
 t: Time.time_t; 
 p: Coord.coordf_t
}

type le_tab_state_t = enc_t option array 

let enc ~time ~place = {t = time; p = place}
let enc_age  m = (Time.get_time ()) -. m.t

class le_tab ~ntargets = 
object(s)

  val enc_arr = Array.make ntargets None

  method add_encounter ~nid ~pos = 
    try 
      let present_time = Time.get_time() in
      enc_arr.(nid) <- Some {t = present_time; p = pos}
    with _ -> raise (Failure "Le_tab.add_encounter : nid not in bounds")

  method le ~nid = 
    try 
      enc_arr.(nid) 
    with _ -> raise (Failure "Le_tab.last_encounter : nid not in bounds")
    
  method le_time ~nid = 
    match s#le ~nid:nid with
	None -> None
      | Some enc -> Some enc.t

  method le_pos ~nid = 
    match s#le ~nid:nid with
	None -> None
      | Some enc -> Some enc.p

  method le_age ~nid = 
    match s#le ~nid:nid with
	None -> max_float
      | Some encounter -> enc_age encounter

  method num_encounters = 
    Array.fold_right
      (fun encopt count -> count + if encopt <> None then 1 else 0) enc_arr 0

  method dump_state = enc_arr

  method load_state dbstate = 
    Array.iteri
      (fun i encopt -> 
	match encopt with
	  | None -> ()
	  | Some enc -> 
	      try enc_arr.(i) <- Some enc
	      with _ -> raise (Failure "Le_tab.load_state : nid not in bounds")

      ) dbstate
end


