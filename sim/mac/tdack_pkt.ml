(*
 *
 *  nab - Network in a Box
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

(* @author Thomas Schmid. *)

type ack = {cost : int}
    (* cost: the cost of the acking node *)
type pkt = {
  min : int;
  max : int;
  num: int
}

type t = ACK of ack | PKT of pkt

let make_ack ~cost = {cost=cost}

let make_pkt ~min ~max ~num = {min=min; max=max; num=num}

let clone p = p

let string_of_tdack_pkt = function
  | ACK {cost=c} -> Printf.sprintf "ACK: cost %d" c
  | PKT p -> Printf.sprintf "PKT: min: %d max: %d num: %d" p.min p.max p.num

let size p = 
  match p with
    | ACK _ -> 1
    | PKT _ -> 3

let tdack_cost p =
  match p with
    | ACK {cost=c} -> c
    | PKT _ -> -1

