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
open Printf
open Misc
open Script_utils


let () = 

  Param.set Params.x_size 100.;
  Param.set Params.y_size 100.;
  Param.set Params.x_pix_size 400;
  Param.set Params.y_pix_size 400;

  Gui_gtk.init();
  let refresh() =   (
    print_endline "refresh\n";
    Gui_gtk.draw_segments_taur [(270,380), (120,20)];
    Gui_gtk.draw_segments_taur [(10,50), (300,300)]
  )

  in
  Gui_gtk.set_expose_event_cb (fun _ -> refresh(); false);
    print_endline "set callback refresh\n";
  
  (*  Gui_grep.create_buttons_grep();*)





  Main.main();

      
