(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

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
let t = ref (Common.get_time())

let rt = ref None (* keep a copy of last route around so expose_event can
		     redraw it *)

let start_stop_btn = ref None
let start_stop_tab:GPack.table option ref = ref None
let ss_btn() = o2v !start_stop_btn
let ss_tab() = o2v !start_stop_tab
let choose_route_btn = ref None
let rt_btn() = o2v !choose_route_btn

let show_nodes = ref true
let show_route_lines = ref true
let show_route_anchors = ref true
let show_route_disks = ref true
let show_connectivity = ref false
let show_tree = ref true


let route_portion = ref 1.0

let run mws_tick display_cb () = (

  t := (Common.get_time());
  let continue() = ((Common.get_time()) < !t +. mws_tick) in
  (Gsched.sched())#run_until~continue;

  display_cb();
  true
)
  

let stop() = (
  Gui_gtk.txt_msg "Nodes are frozen ";


  Timeout.remove (o2v !run_id);
  run_id := None;
)

let startmws ~mws_tick ~rt_tick_ms ~display_cb  = (

  rt := None;
  Gui_gtk.txt_msg "Nodes are moving ";
  Mob_ctl.start_all();
  let do_one_cycle = run mws_tick display_cb in
  ignore(do_one_cycle());
  run_id := Some (Timeout.add ~ms:rt_tick_ms ~callback:do_one_cycle);
)




let get_node_cb_sig_id = ref None

let remove_get_node_cb() = 
  let id = o2v !get_node_cb_sig_id in
  Gui_gtk.remove_button_press_cb id




    
(* to kill: window#destroy ()*)

