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








let background_of_string = function
  | "epfl" | "Epfl" | "EPFL" -> Epfl.epfl_xpm
  | "blank" | "Blank" -> Blank.blank_xpm
  | _ -> raise (Failure "Invalid format for background graphic.")

let string_of_background = function
  | b when (b = Epfl.epfl_xpm) -> "EPFL"
  | b when (b = Blank.blank_xpm) -> "Blank"
  | _ -> failwith "Params_gui.string_of_backgound"

let xpm_bg = Param.create
  ~name:"background"
  ~default:Blank.blank_xpm
  ~doc:"Background graphic"
  ~printer:string_of_background
  ~reader:background_of_string
  ()

