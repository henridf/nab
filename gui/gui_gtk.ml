(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Coord
open Misc
open GMain

(*let cl_fg = `NAME "blue" *)
let cl_fg = `NAME "black"
let cl_hilite = `NAME "red"

let wnd = ref None 
let (vbx : GPack.box option ref) = ref None
let vbox() = o2v !vbx

let pkr = ref (fun _ -> ())
let window () = o2v !wnd 

let gdk_wnd = ref None 
let gdk_window () = o2v !gdk_wnd

let (drw : [ `window] GDraw.drawable option ref) = ref None 
let drawing () = o2v !drw


let packer() =  !pkr

let (fixed : GPack.fixed option ref) = ref None
let fix() = o2v !fixed
let txt_label = ref None
let txt() = o2v !txt_label
let pxm = ref None
let pixmap() = o2v !pxm




let res = 10
  (* using list of functions is not most efficient, but we are assuming there
     will be a small number of functions per list. Otherwise CPS might be nice
     here *)
let draw_array = ref [||]



let copy_pixmap x y = (
  try 
    (drawing())#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width:res
    ~height:res   (pixmap()) 
  with 
    | Failure "Misc.o2v : None"
      -> ()
    | e -> raise e
)

let draw ~clear () = (
  Array.iteri (
    fun x column ->
      Array.iteri (
	fun y funs -> (
	  if (clear) then (
	    copy_pixmap (x*res) (y*res);
	  );
	  List.iter (fun f -> f()) funs;
	  !draw_array.(x).(y) <- [(fun () -> ()) ];
	  
	)
      )  column
  ) !draw_array 
)
  
  
  

let expose_cb_id = ref None 
let set_expose_event_cb f = (
  if !expose_cb_id <> None then
    (fix())#misc#disconnect (o2v !expose_cb_id);
  
  expose_cb_id := (Some ((fix())#event#connect#expose f))
)

let init () = (

  Gui_hooks.init();

  let width = (Param.get Params.x_pix_size) 
  and height = (Param.get Params.y_pix_size)  in

  draw_array := Array.make_matrix (width/res) (height/res) [(fun () -> ()) ];

  wnd := Some (GWindow.window ~show:true ());

  ignore ((window())#connect#destroy ~callback:Main.quit);
  
  vbx := Some (GPack.vbox ~packing:(window())#add ());
  pkr := (vbox())#add;
  fixed := Some (GPack.fixed ~width ~height ~packing:((vbox())#add) ());

  gdk_wnd := Some ((fix())#misc#realize (); (fix())#misc#window);

  drw := Some (new GDraw.drawable (gdk_window()));

(*
  let (pixmap_, bitmap) = Gdk.Pixmap.create_from_xpm_d
    ~data:Epfl.epfl_xpm
    ~window:(gdk_window())
    ()
  in
  pxm := Some pixmap_;

  Gdk.Window.set_back_pixmap (gdk_window()) (`PIXMAP pixmap_);
*)

 txt_label := Some (GMisc.label ~text:"" ~packing:(packer()) ());

)

  
let install_button_press_cb cb = (
  (fix())#event#connect#button_press 
    ~callback:cb
)

let remove_button_press_cb id = (
  (fix())#misc#disconnect id
)

let txt_msg msg = (
  (txt())#set_text msg
)

let boundarize pos = (
  let width = (Param.get Params.x_pix_size) 
  and height = (Param.get Params.y_pix_size)  in

  let newx = ref (xx pos) and newy = ref (yy pos) in 
  newx := (max 0 !newx);
  newx := (min (width - 1) !newx);

  newy := (max 0 !newy);
  newy := (min (height - 1) !newy);
  (!newx, !newy)
)

let draw_segments_buf ?(col=cl_fg) ?(thick=1) s = (


  List.iter 
  (fun (p1, p2) ->
	let f() = 
	  (drawing())#set_line_attributes ~width:thick ();
	  (drawing())#set_foreground col;
	  (drawing())#segments s;
	  (drawing())#set_foreground cl_fg;    
	  (drawing())#set_line_attributes ~width:1 ();

	in
	let (g1x, g1y) = (boundarize p1) /// res
	and (g2x, g2y) = (boundarize p2) /// res
	in
	match (g1x, g1y) = (g2x, g2y) with
	  | true -> !draw_array.(g1x).(g1y) <- f::!draw_array.(g1x).(g1y)
	  | false -> 
	      !draw_array.(g1x).(g1y) <- f::!draw_array.(g1x).(g1y);
	      !draw_array.(g2x).(g2y) <- f::!draw_array.(g2x).(g2y)
  ) s
)

let draw_cross ?(diag=true) ?(col=cl_fg) ?(target=false) (x, y) = (
  let (pixwid, thick) = if target then (4, 2) else (2, 1) in
  let segs = if diag then 
    [
      (x - pixwid, y - pixwid), (x + pixwid + 1, y + pixwid + 1);
      (x + pixwid + 1, y - pixwid - 1), (x - pixwid, y + pixwid)
    ]
  else
    [
      (x, y - pixwid), (x, y + pixwid);
      (x - pixwid, y), (x + pixwid, y)
    ]
  in
   draw_segments_buf ~col ~thick segs
)  

let draw_segments ?(col=cl_fg) l = (
  (drawing())#set_foreground col;
  (drawing())#segments l;
  (drawing())#set_foreground cl_fg;    
)

let draw_node ?(col=cl_fg) ?(target=false) pos = (
  draw_cross ~col ~target pos;
)


let draw_nodes pos_list =  
  List.iter (fun p -> draw_node p) pos_list

let draw_circle ~centr ~radius = 
  (drawing())#arc
  ~x:((fst centr) - radius ) 
    ~y:((snd centr) - radius) 
    ~width:(radius * 2)
    ~height:(radius * 2) 
    ()
    


(* (for future reference)
   how to draw on a pixmap, then display the pixmap:
   

  wnd := Some (GWindow.window ~show:true ());

  let _ = (window())#connect#destroy ~callback:Main.quit in

  
  let vbox = GPack.vbox ~packing:(window())#add () in
  pkr := vbox#add;
  fixed := Some (GPack.fixed ~width:1200 ~height:900 ~packing:(vbox#add) ());

  gdk_wnd := Some ((fix())#misc#realize (); (fix())#misc#window) ;

   drw2 := Some (new GDraw.drawable (gdk_window()));

  let (pixmap_, bitmap) = Gdk.Pixmap.create_from_xpm
    ~file:"/tmp/epfl.xpm" 
    ~window:(gdk_window())
    ()
  in
  pxm := Some pixmap_;
  drw := Some ((new GDraw.pixmap) pixmap_);


(* now drw is a drawable so can do #segments, etc on it*)
   
(* not we put the obtained pixmap on the background pixmap *)

  (drawing2())#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 ~width:1200 ~height:900
  (drawing())#pixmap
*)
