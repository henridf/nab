(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Coord
open Misc
open GMain

let cl_fg = `NAME "blue" 
let cl_bg = `BLACK



let wnd = ref None 
let gdk_wnd = ref None 
let drw = ref None 
let window () = o2v !wnd 
let gdk_window () = o2v !gdk_wnd 
let drawing () = o2v !drw

let q = Linkedlist.create() 

let redraw _ = (
  Printf.printf "Redrawing\n"; flush stdout;
  Linkedlist.iter (fun f -> f()) q;
  true;
)

let clear() = 
  Gdk.Window.clear (gdk_window())

let init () = (
  wnd := Some (GWindow.window ~show:true ());

  let _ = (window())#connect#destroy ~callback:Main.quit in

  
  let fix = GPack.fixed ~width:1200 ~height:900 ~packing:((window())#add) () in
  gdk_wnd := Some (fix#misc#realize (); fix#misc#window) ;
  drw := Some (new GDraw.drawable (gdk_window()));

  let (pixmap, bitmap) = Gdk.Pixmap.create_from_xpm
    ~file:"/home/henri/work/150th/epfl.xpm" 
    ~window:(gdk_window())
    ()
  in
  Gdk.Window.set_back_pixmap fix#misc#window (`PIXMAP pixmap);
  
(*  ignore (fix#event#connect#expose ~callback:redraw);*)
(*  ignore (Timeout.add ~ms:100 ~callback:redraw)*)

)

let draw_segs s = (
(*  (window())#misc#draw None;
  (drawing())#misc#draw None;*)
  Linkedlist.insert 
    ~ll:q 
    ~v:(fun _ ->   (drawing())#segments s) 
    ~compare:(fun _ _ -> true);

)

(* pixcenter in gtk pixels *)

let draw_cross (x, y) = 
  let pixwid = 3 in
  draw_segs 
    [
      (x, y - pixwid), (x, y + pixwid);
      (x - pixwid, y), (x + pixwid, y)
    ]

let erase draw_action = (
  (drawing())#set_foreground cl_bg;
  draw_action();
  (drawing())#set_foreground cl_fg;
)

let draw_segments l = 
  draw_segs l

let draw_node pos = 
  Linkedlist.insert 
    ~ll:q 
    ~v:(fun _ -> draw_cross pos) 
    ~compare:(fun _ _ -> true)
(*  draw_cross ~centerpix:pos*)

let undraw_node ~mwspos = erase (fun _ -> draw_node mwspos)


let draw_nodes pos_list =  
  
  List.iter (fun p -> draw_node p) pos_list


