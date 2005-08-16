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

let _ = 

  Script_utils.parse_args();
  
  let dumpfile = 
    try 
      (Param.get Script_params.dumpfile)
    with Param.NoParamVal _ -> "/doesntexist"
  in
  
  
  if not (Sys.file_exists dumpfile) then 
    Warmup_utils.setup_sim ()
  else
    Warmup_utils.restore_sim dumpfile;

  
  
  Nodes.iter (fun n -> n#remove_mac ());
  Script_utils.install_cheat_macs();
  
  Mob_ctl.stop_all ();
  Param.set Params.x_pix_size 700; (* size of gui window *)
  Param.set Params.y_pix_size 700;
  Gui_gtk.init();
  Gui_ler.setup_easeviz_app();
  GMain.Main.main();
