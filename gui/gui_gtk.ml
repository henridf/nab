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
let draw_array = Array.make_matrix (1200/res) (900/res) [(fun () -> ()) ]
let copy_pixmap x y = (
  (drawing())#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width:res
  ~height:res   (pixmap()) 
)

let draw() = (
  Array.iteri (
    fun x column ->
      Array.iteri (
	fun y funs -> (
	  copy_pixmap (x*res) (y*res);
	  List.iter (fun f -> f()) funs;
	  draw_array.(x).(y) <- [(fun () -> ()) ];
	)
      )  column
  ) draw_array 
)


let clear() = (

  draw();


)




let init () = (
  wnd := Some (GWindow.window ~show:true ());

  let _ = (window())#connect#destroy ~callback:Main.quit in

  
  vbx := Some (GPack.vbox ~packing:(window())#add ());
  pkr := (vbox())#add;
  fixed := Some (GPack.fixed ~width:1200 ~height:900 ~packing:((vbox())#add) ());

  gdk_wnd := Some ((fix())#misc#realize (); (fix())#misc#window);

  drw := Some (new GDraw.drawable (gdk_window()));

  let (pixmap_, bitmap) = Gdk.Pixmap.create_from_xpm_d
    ~data:Epfl.epfl_xpm
    ~window:(gdk_window())
    ()
  in
  pxm := Some pixmap_;

  Gdk.Window.set_back_pixmap (gdk_window()) (`PIXMAP pixmap_);

 txt_label := Some (GMisc.label ~text:"Go on!" ~packing:(packer()) ());

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

let draw_segs s = (


  List.iter 
  (fun (p1, p2) ->
	let f() = (drawing())#segments s
	in
	let (g1x, g1y) = p1 /// res
	and (g2x, g2y) = p2 /// res
	in
	match (g1x, g1y) = (g2x, g2y) with
	  | true -> draw_array.(g1x).(g1y) <- f::draw_array.(g1x).(g1y)
	  | false -> 
	      draw_array.(g1x).(g1y) <- f::draw_array.(g1x).(g1y);
	      draw_array.(g2x).(g2y) <- f::draw_array.(g2x).(g2y)
  ) s

)


(* pixcenter in gtk pixels *)

let draw_cross (x, y) = 
  let pixwid = 3 in
  draw_segs 
    [
      (x, y - pixwid), (x, y + pixwid);
      (x - pixwid, y), (x + pixwid, y)
    ]


let draw_segments l = 
  draw_segs l

let draw_node hilite pos = (
  if hilite then 
()
(*    (drawing())#set_foreground cl_hilite*)
 else 
   (drawing())#set_foreground cl_fg;    
  
  
  draw_cross pos;

)


let draw_nodes pos_list =  
  List.iter (fun p -> draw_node false p) pos_list

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
