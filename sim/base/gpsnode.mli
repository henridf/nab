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



(** Gpsnode: A node with all functionality of a {!Simplenode.simplenode}, and which also
  has knowledge of its position. This can be used by geographic routing
  agents for example.

  @author Henri Dubois-Ferriere. 
*)


class gpsnode :  Common.nodeid_t ->
	  
object
  inherit Simplenode.simplenode
    
  method pos : Coord.coordf_t
    (** Returns the position of this node. *)

    (**/**)
  method coerce : Simplenode.simplenode
    (* This was only to untangle a circular dependency between files, which
       was happening when we were doing :> Simplenode.simplenode directly in
       nodes.ml *)
end
  
	  
  
  
