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


open GMain
open Misc
open Script_utils

let dumpfile = Param.stringcreate ~name:"dumpfile" 
  ~cmdline:true
  ~doc:"File to dump warmup state"
  ()
  

let sp = Printf.sprintf

let detach = Param.boolcreate 
  ~name:"detach" 
  ~doc:"Detach from terminal"
  ~cmdline:true
  ~default:false
  ~notpersist:true
  ()
  
let () = 

  Warmup_utils.setup_or_restore();
  if Param.get detach then 
    Script_utils.detach_daemon();
  Warmup_utils.maybe_warmup (Param.get dumpfile)
