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


open Misc
open Script_utils

(*  dumps gradient to stdout as list of <distance, age> tuples *)
let dump_gradient ?(dst=0) () = 
  let g = Ler_utils.dist_age_arr () in
  Array.iter (fun (dst, age) -> Printf.printf "%f %f\n" dst age) g



let () = 

  Script_utils.parse_args();
  Arg.current := 0;

  let dumpfile = Param.get Script_params.dumpfile in
  
  (* restore from dumpfile if dumpfile argument provided. *)
  if not (Sys.file_exists dumpfile) then 
    Warmup_utils.setup_sim ()
  else
    Warmup_utils.restore_sim dumpfile;

  
  if Param.get Script_params.detach then begin
    let logname = (Filename.chop_extension dumpfile)^".log" in
    Script_utils.detach_daemon ~outfilename:logname ()end;
  
  (* warmup if -warmup flag was passed *)
  Warmup_utils.maybe_warmup dumpfile;
  dump_gradient ()

    
