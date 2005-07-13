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



type 'a t = Empty | Node of 'a * 'a t list
exception Empty_error
exception Duplicate_node

let rec belongs el =  function
  | Empty -> false
  | Node (parent, children) -> (parent = el) || (List.exists (belongs el) children)

let rec depth = 
  let max_list l = List.fold_left max 0 l in
  function 
    | Empty -> raise Empty_error
    | Node (_, children) -> 1 + (max_list (List.map depth children))

let rec iter f = 
  function
    | Empty -> ()
    | Node (parent, children) -> f parent; List.iter (iter f) children
let size t = 
  let count = ref 0 in
  iter (fun _ -> incr count) t;
  !count

let iter2 f tree = 
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
  
let rec leaves_aux acc (tree : 'a t) = 
  match tree with
    | Empty -> []
    | Node (parent, []) -> parent::acc
    | Node (parent, children) -> (List.fold_left (leaves_aux) [] children)@acc
  

let leaves tree = leaves_aux [] tree

let parent_hash tree = 
  let h  = Hashtbl.create (size tree) in
  iter2 (fun ~parent ~child -> Hashtbl.add h child parent) tree;
  h

let paths tree = 
  let l = leaves tree in 
  let h = parent_hash tree in
  let rec path leaf = 
    if Hashtbl.mem h leaf then leaf::(path (Hashtbl.find h leaf))
    else [leaf]
  in
  List.map path l

let root = function
  | Empty -> raise Empty_error
  | Node (v, _) -> v

let rec is_ancestor ~ancestor ~node tree = (
  let nodes_belong = ref (belongs ancestor tree) in
  let h = parent_hash tree in
  let parent_node = 
    try (Hashtbl.find h node) with 
	Not_found -> (
	  nodes_belong := false;
	  root tree
	)
  in
  if !nodes_belong = true then (
    if parent_node = ancestor then true else 
      if parent_node = (root tree) then false
      else  is_ancestor ~ancestor ~node:parent_node tree
  ) else false
)

let successors node tree = (
  let succ = ref [] in
  iter (fun i -> 
    if(is_ancestor ~ancestor:node ~node:i tree) then 
      succ := !succ @ [i];) 
    tree;
  !succ
)
	
let rec map ~f = 
  function 
    | Empty -> Empty
    | Node (parent, children) -> Node (f parent, List.map (map ~f) children)
	
let addnode ~parent ~node tree = 
  if belongs node tree then raise Duplicate_node;
  let addtolist l = (Node (node, []))::l in
  
  let rec add_ = function 
    | Empty -> Node (node, [])
    | Node (v, subtree) when (v = parent) ->
	Node (v, addtolist subtree)
    | Node (v, subtree) -> Node (v, List.map add_ subtree)
  in
  add_ tree 

let sprintf ~f tree = 
  match size tree with
    | 0 -> "Tree is empty"
    | 1 -> Printf.sprintf "Single node %s" (f (root tree))
    | k -> print_string "number of nodes: "; print_int k; print_newline();
	let buf = Buffer.create 50 in
	let printnode ~parent ~child = 
	  Buffer.add_string buf
	    (Printf.sprintf "%s has parent %s\n" (f child) (f parent)) in
	iter2 printnode tree;
	Buffer.contents buf
