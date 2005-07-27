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

(* dumpconf: Simple script to dump the configuration contained in a 'frozen'
   simulation dump file. 
   This is helpful to figure things out when one has a bunch of previous dump
   files and we've lost track of what was in each one... *)



let usage() = 
  Printf.printf "dumpconf frozensim\n";
  Printf.printf "   Outputs the configuration of the frozen simulation contained in file 'frozensim'\n";
  exit 1

let nofile() = 
  Printf.printf "Cannot find file %s\n" Sys.argv.(1);
  exit 1


let _ = 

  if Array.length Sys.argv <> 2 then usage();
  let fname = Sys.argv.(1) in
  if not (Sys.file_exists fname) then nofile();
  let ic = (open_in fname) in
  Persistency.get_config ic;
  close_in ic;
