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

(* $Id$ *)



type 'a t = Empty | Node of 'a * 'a t list

let rec belongs el =  function
  | Empty -> false
  | Node (parent, children) -> (parent = el) || (List.exists (belongs el) children)

let rec height = 
  let max_list l = List.fold_left max 0 l in
  function 
    | Empty -> 0
    | Node (_, children) -> 1 + (max_list (List.map height children))

let rec iter ~f = 
  function
    | Empty -> ()
    | Node (parent, children) -> f parent; List.iter (iter ~f) children

let size t = 
  let count = ref 0 in
  iter ~f:(fun _ -> incr count) t;
  !count

let iter2 ~f tree = 
  let myf ~parent ~child = 
    match child with
      | Empty -> ()
      | Node (c, _) -> f ~parent ~child:c
  in
  let rec iter2_ = 
    function
    | Empty -> ()
    | Node (parent, children) -> List.iter (fun child -> myf ~parent ~child) children; List.iter iter2_ children
  in iter2_ tree
  

let root = function
  | Empty -> failwith "root"
  | Node (v, _) -> v

	
let rec map ~f = 
  function 
    | Empty -> Empty
    | Node (parent, children) -> Node (f parent, List.map (map ~f) children)
	
let addnode ~parent ~node tree = 
  if belongs node tree then failwith "addnode";
  let addtolist l = (Node (node, []))::l in
  
  let rec add_ = function 
    | Empty -> Node (node, [])
    | Node (v, subtree) when (v = parent) ->
	Node (v, addtolist subtree)
    | Node (v, subtree) -> Node (v, List.map add_ subtree)
  in
  add_ tree 

