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





(* each node has a value, and a foreward pointer *)
type 'a node = { 
  mutable v : 'a;
  mutable fore : 'a node option 
}

type 'a llist =  'a node option ref
    
let create () = ref None


let insert ~ll ~v ~compare = 
  let rec advance node = 
    match node.fore with
      | None -> node.fore <- Some {v=v; fore=None}
      | Some nextnode -> 
	  match compare  v nextnode.v with
	    | true -> node.fore <- Some {v=v; fore=Some nextnode}
	    | false -> advance nextnode
  in
  match !ll with 
    | None -> ll := Some {v=v; fore=None} (* empty *)
    | Some head -> 
	match compare v head.v with (* One node *)
	  | true -> ll := Some {v=v; fore = Some head};
	  | false -> advance head (* > 1 node *)

let iter f ll = 
  let rec advance node = 
    f node.v;
    match node.fore with
      | None -> ()
      | Some nextnode -> 
	  advance nextnode
  in 
  match !ll with 
    | None -> () 
    | Some head ->
	advance head


let pophead ~ll = 
match !ll with 
  | None -> None   (* empty *)
  | Some head -> ( 
      match head.fore with
	| None -> ll := None (* One node *)
	| Some next -> ll := Some {v=next.v; fore=next.fore}; (* > 1 node *)
    );
      Some head.v

  
