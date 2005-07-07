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

open Common
open Misc

type worldtype = 
  | Lazy
  | Greedy
  | Lazy_taurus 
  | Greedy_taurus 
  | Epfl

type world_dim =  One | Two

let (lazy_world_ : Worldt.lazy_world_t option ref) = ref None 
let w () = try o2v !lazy_world_ with
  | Failure _ -> raise (Failure "World.w() : no instance has been set")
let set_lazy_world t = lazy_world_ := Some t

let (greedy_world_ : Worldt.greedy_world_t option ref) = ref None 
let gw () = try o2v !greedy_world_ with
  | Failure _ -> raise (Failure "World.gw() : no instance has been set")

let set_greedy_world t = 
  greedy_world_ := Some t; 
  lazy_world_ := Some (t :> Worldt.lazy_world_t)

let world_of_string str = 
  let l = Str.split (Str.regexp ",") str in
  if List.length l <> 2 then raise (Failure ("Invalid worldtype "^str));
  let worldtype = begin match List.hd l with
  | "lazy_taurus" |  "taurus_lazy" | "tl" | "lt"
      -> Lazy_taurus
  | "greedy_taurus" |  "taurus_greedy" | "tg" | "gt"
      -> Greedy_taurus
  | "lazy" | "l"
      -> Lazy
  | "greedy" |  "g" 
      -> Greedy
  | "epfl" 
    -> Epfl
  | s -> raise (Failure ("Invalid worldtype "^s))
  end 
  and worlddim = begin match List.nth l 1 with
    | "1" | "one" | "1d" | "1-d" | "onedim" | "one-dim" | "1dim" | "1-dim" ->
	One
    | "2" | "two" | "2d" | "2-d" | "twodim" | "two-dim" | "2dim" | "2-dim" ->
	Two
  | s -> raise (Failure ("Invalid worldtype "^s))
end
  in 
  worldtype, worlddim

let string_of_world (wt, wd) = 
  begin match wt with
    | Greedy -> "greedy"
    | Lazy -> "lazy"
    | Lazy_taurus -> "lazy_taurus" 
    | Greedy_taurus -> "greedy_taurus"
    | Epfl -> "epfl" 
  end
  ^ "," ^
  begin match wd with 
    | One -> "1"
    | Two -> "2"
  end

let world = 
  Param.create 
    ~name:"world" 
    ~cmdline:true
    ~default:(Lazy, Two) 
    ~doc:"World type"
    ~reader:world_of_string 
    ~printer:string_of_world()

	
