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







(* Notes on hacks/changes for quick cens dirty job.
   create_buttons_trees was just a cutnpaste/change of create_buttons_ease - 
   no attempt at figuring out how things could be less hardcoded and more
   fnexible. same for set_src_trees w.r.t set_src.
   (basically we shoul distinguish btw generic stuff like offering a
   fun to pick a node, etc, and stuff which is app-specific (like drawing a
   route (set_src below), etc).

   in install_get_node_cb() , simply replaced the call to set_src with
   set_tree_src. 


*)

   


open Misc
open GMain

let run_id = ref None
let t = ref (Time.get_time())


let run sim_tick display_cb () = (

  t := (Time.get_time());

  let continue() = ((Time.get_time()) < !t +. sim_tick) in
  (Sched.s())#run_until~continue;

  display_cb();
  true
)
  

let stop() = (

  Timeout.remove (o2v !run_id);
  run_id := None;
)

let startsim ~sim_tick ~rt_tick_ms ~display_cb  = (

  let do_one_cycle = run sim_tick display_cb in
  ignore(do_one_cycle());
  run_id := Some (Timeout.add ~ms:rt_tick_ms ~callback:do_one_cycle);
)




let get_node_cb_sig_id = ref None

let remove_get_node_cb() = 
  let id = o2v !get_node_cb_sig_id in
  Gui_gtk.remove_button_press_cb id




    
(* to kill: window#destroy ()*)

