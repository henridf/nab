(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc
open GMain

let run_id = ref None
let t = ref (Common.get_time())
let start_stop_btn = ref None
let start_stop_tab:GPack.table option ref = ref None
let ss_btn() = o2v !start_stop_btn
let ss_tab() = o2v !start_stop_tab

let run() = (
(*
      while (Glib.Main.pending()) do
	ignore (Glib.Main.iteration false)
      done;
*)

	t := (Common.get_time());
	let continue() = ((Common.get_time()) < !t +. 1.0) in
	(Gsched.sched())#run_until~continue;
(*	Gui_gtk.clear();*)
	Gui_ops.draw_all_nodes(); 
(*	ignore(Gui_gtk.redraw());**)

	true
)
  

let start_stop () = (
  match !run_id with
    | Some id ->
	Timeout.remove (id);
	run_id := None
    | None ->
	ignore(run());
	run_id := Some (Timeout.add ~ms:100 ~callback:run);
	
)

let make_ss_btn() = (
  if !start_stop_btn != None then
    (ss_btn())#destroy();

  start_stop_btn :=  Some (GButton.toggle_button ~draw_indicator:false
    ~label:"start/stop" ()) ;
  ignore ((ss_btn())#connect#released ~callback:(start_stop));
  (ss_tab())#attach ((ss_btn())#coerce) ~left:0 ~top:0 ~right:1 ~bottom:1
  ~xpadding:0 ~ypadding:0  ~expand:`BOTH;
)

let create_buttons() = (

  start_stop_tab := Some (GPack.table ~rows:2 ~columns:2 ~homogeneous:false 
    ~row_spacings:3 ~col_spacings:3 ~border_width:10
    ~packing:(Gui_gtk.packer()) ());
  make_ss_btn();

  )    
(* to kill: window#destroy ()*)

