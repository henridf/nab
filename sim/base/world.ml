(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)








open Common
open Misc

type worldtype = 
    Lazy_taurus 
  | Greedy_taurus 
  | Lazy_reflecting 
  | Greedy_reflecting
  | Epfl

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


let str2mac s = 
    match s with 
      | "lazy_taurus" |  "taurus_lazy" 
	  -> Lazy_taurus
      | "greedy_taurus" |  "taurus_greedy" 
	  -> Greedy_taurus
      | "lazy_reflecting" |  "reflecting_lazy" 
	  -> Lazy_reflecting
      | "greedy_reflecting" |  "reflecting_greedy" 
	  -> Greedy_reflecting
      | "epfl" |  "epfl_world" 
	  -> Epfl
      | _ -> raise (Failure ("Invalid worldtype "^s))

