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



(** Tweaked and slightly augmented versions of the standard library modules.

  @author Henri Dubois-Ferriere.

*)

 module Array = struct

   include Array

   let filter test a =
     let result = (Array.fold_left
       (fun accu elt ->
	 if test elt then elt :: accu else accu)
       [] a) in
     Array.of_list (List.rev result)

   let iter a b = Array.iter b a
   let iteri a b = Array.iteri b a
   let map a b = Array.map b a
  end



 module List = struct

   include ListLabels

   let iteri ~f l = 
     let rec iteri_ l i = 
       match l with 
	 | [] -> ()
	 | a::l -> f a i; iteri_ l (i + 1) in
     iteri_ l 0;;
   
  end


 module String = struct
   include String
   let last s = s.[String.length s - 1]
 end
