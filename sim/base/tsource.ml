(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)






open Misc


let rndgen = Random.State.copy (Random.get_state())

let make_finite_trafficsource f num_pkts = 
    let ct_ = ref 0 in 
    let f_ = fun()  -> (
      incr ct_;   if !ct_ < num_pkts then Some (f())  else None)
    in
    f_


let make_cbr ?num_pkts ~pkts_per_sec () = 
  let time_to_next_pkt() = 1.0 /. pkts_per_sec in
  match num_pkts with 
    | None -> fun () -> Some (time_to_next_pkt())
    | Some n ->  make_finite_trafficsource time_to_next_pkt n
  
    
    
let make_poisson ?num_pkts ~lambda () =
  let time_to_next_pkt() = 
    let rand = Random.State.float rndgen 1.0 in
    Misc.expo ~rand ~lambda
  in
  match num_pkts with 
    | None -> fun () -> Some (time_to_next_pkt())
    | Some n -> make_finite_trafficsource time_to_next_pkt n
    
