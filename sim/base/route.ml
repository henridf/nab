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



open Mods



(* Simple list-based representation of a LER route. *)


open Common
open Printf

type ('a, 'b) hop_t = {hop:'a; mutable info:'b option}

type ('a, 'b) t = ('a, 'b) hop_t list


type 'a hops_only_route_t = ('a, unit) t



let create () = []


let add_hop path hop = path @ [hop]
  (* according to lib/ocaml/list.mli, non-tail recursion can be ignored when <
     10000 elements, which a route is almost certain be *)

let append_hops ~front ~back = front @ back

let nth_hop path n = List.nth path n
let last_hop path = Misc.listlast path

let length path = List.length path


let i2c route = (
  List.map 
  ~f:(fun h -> 
    {h with hop=(World.w())#nodepos h.hop})
    route
)



let eucl_length ~dist_f route = (
  let rec recurse_ r len = 
    match r with 
      | [_] -> len
      | a::b::c -> recurse_ (b::c) (len +. (dist_f a.hop b.hop))
      | [] -> raise (Misc.Impossible_Case "Route.eucl_length")
  in
recurse_ route 0.0
)



let sprint_hops f route = (
  String.concat 
  ("\n")
  (List.map 
    route 
    ~f:(fun x -> Printf.sprintf "hop:%s" (f x.hop) ))

) 

let sprintf_hopsonly route = sprint_hops Coord.sprintf route
let sprinti_hopsonly route = sprint_hops Coord.sprint route
let sprintnid_hopsonly route = sprint_hops Misc.i2s route



