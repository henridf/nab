(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc

let running = ref false
let t = ref (Unix.gettimeofday())
let ctl_wnd = ref None
let start_stop_btn = ref None
let start_stop_tab:GPack.table option ref = ref None
let ss_btn() = o2v !start_stop_btn
let ss_tab() = o2v !start_stop_tab

let run() = (
  Printf.printf "running\n"; flush stdout;

  try 
    while true do
      while (Glib.Main.pending()) do
	ignore (Glib.Main.iteration false)
      done;
      if (!running) then (
	t := (Unix.gettimeofday());
	let continue() = (Unix.gettimeofday() < !t +. 0.01) in
	(Gsched.sched())#run_until~continue;
(*	ignore(Gui_gtk.redraw());*)
	Gui_gtk.clear();
      ) else (
	raise Misc.Break
      )
    done;
  with 
    | Break -> (Gsched.sched())#stop_at ~t:Sched.ASAP
    | o -> raise o
)
  

let start_stop () = (
  match !running with
    | true ->
	running := false
    | false ->
	running := true;
	run()

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
  let window = GWindow.window ~title: "Controls" ~border_width: 0 () in
  ignore (window#connect#destroy ~callback:(fun () -> ()));

  let box1 = GPack.vbox ~packing:window#add () in
  
  start_stop_tab := Some (GPack.table ~rows:2 ~columns:2 ~homogeneous:false 
    ~row_spacings:3 ~col_spacings:3 ~border_width:10
    ~packing:box1#add ());

  make_ss_btn();
  window#show ()
  )    
(* to kill: window#destroy ()*)

