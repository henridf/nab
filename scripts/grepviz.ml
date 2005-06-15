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


let sp = Printf.sprintf

  
let () = 

  Warmup_utils.setup_or_restore (Param.get Script_params.dumpfile);

  Pervasives.at_exit (fun () ->
    let stats = Warmup_utils.sprint_added_stats() in
    print_string "\n\n";
    print_string stats;
  );


(*  Warmup_utils.maybe_warmup();*)
  
  
  (* (Sched.s())#run();
     install_tsources();
     (Sched.s())#run_for ~duration:3000.;
     exit 0;

     let dst = 0 in
     for i = 0 to -1 do 
     (Nodes.node dst)#originate_app_pkt ~l4pkt:`EMPTY ~dst:(i + 2);

     done;
  *)
  

  Param.set Params.x_pix_size 600;
  Param.set Params.y_pix_size 600;

  Gui_gtk.init();
  Gui_grep.setup_grepviz_app();
  Main.main();

