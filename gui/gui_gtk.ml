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



open Coord
open Misc
open GMain

(*let cl_fg = `NAME "blue" *)
let cl_fg = `NAME "black"
let cl_hilite = `NAME "red"

let wnd = ref None 
let window () = o2v !wnd 

let gdk_wnd = ref None 
let gdk_window () = o2v !gdk_wnd

let (drw : [ `window] GDraw.drawable option ref) = ref None 
let drawing () = o2v !drw

let (hbx : GPack.box option ref) = ref None
let hbox() = o2v !hbx
let (vbx : GPack.box option ref) = ref None
let vbox() = o2v !vbx

let hpkr = ref (fun _ -> ())
let vpkr = ref (fun _ -> ())

let hpacker() =  !hpkr
let vpacker() =  !vpkr

let (fixed : GPack.fixed option ref) = ref None
let fix() = o2v !fixed
let txt_label = ref None
let txt() = o2v !txt_label
let pxm = ref None
let pixmap() = o2v !pxm


let clear () = 	
  Opt.may
    (fun pixmap -> 
      (drawing())#put_pixmap  ~x:0 
      ~y:0 
      ~xsrc:0 
      ~ysrc:0 
      ~width:(Param.get Params.x_pix_size)
      ~height:(Param.get Params.x_pix_size)
      pixmap
    )
    !pxm


let expose_cb_id = ref None 
let set_expose_event_cb f = (
  if !expose_cb_id <> None then
    (fix())#misc#disconnect (o2v !expose_cb_id);
  
  expose_cb_id := (Some ((fix())#event#connect#expose f))
)
  
let init () = (

  Gui_conv.init();

  let width = (Param.get Params.x_pix_size) 
  and height = (Param.get Params.y_pix_size)  in

  wnd := Some (GWindow.window ~show:true ());

  ignore ((window())#connect#destroy ~callback:Main.quit);
  
  vbx := Some (GPack.vbox ~packing:(window())#add ());
  vpkr := (vbox())#add;

  hbx := Some (GPack.hbox ~packing:(vpacker()) ());
  hpkr := (hbox())#add;

  fixed := Some (GPack.fixed ~width ~height ~packing:((hbox())#add) ());


  gdk_wnd := Some ((fix())#misc#realize (); (fix())#misc#window);

  drw := Some (new GDraw.drawable (gdk_window()));



  let (pixmap_, bitmap) = Gdk.Pixmap.create_from_xpm_d
    ~data:(Param.get Params_gui.xpm_bg)
    ~window:(gdk_window())
    ()
  in
  pxm := Some pixmap_;

  Gdk.Window.set_back_pixmap (gdk_window()) (`PIXMAP pixmap_);

  txt_label := Some (GMisc.label ~text:"" ~packing:(vpacker()) ());
  ignore (GMisc.separator `HORIZONTAL ~packing:(vpacker()) ())
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


let draw_segments ?(col=cl_fg) ?(thick=1) l = (
  (drawing())#set_foreground col;
  (drawing())#set_line_attributes ~width:thick ();
  (drawing())#segments l;
  (drawing())#set_foreground cl_fg;    
  (drawing())#set_line_attributes ~width:1 ();
)

(* compute (vx,vy), the vector going from u to w in a taurus of size a,b *)
let v (ux, uy) (wx, wy) = 
  let a = (Param.get Params.x_pix_size) and b = (Param.get Params.y_pix_size)  
  in
  let vx = match abs(wx - ux) <= a/2 with 
    | true -> wx - ux
    | false -> (a - abs(ux - wx)) * signi (ux - wx)
  and vy = match abs(wy - uy) <= b/2 with 
    | true -> wy - uy
    | false -> (b - abs(uy - wy)) * signi (uy - wy)
  in vx , vy
    
let draw_segment_taur (p1,p2) = 
  print_endline "hello";
  Printf.printf "%s to %s\n"
    (Coord.sprint p1) 
    (Coord.sprint (p1 +++ (v p1 p2)));
  Printf.printf "%s to %s\n"
    (Coord.sprint p2)
    (Coord.sprint (p2 +++ (v p2 p1)));
  flush stdout;
  draw_segments [(p1, p1 +++ (v p1 p2)); (p2, p2 +++ (v p2 p1))]


let draw_segments_taur ?(col=cl_fg) ?(thick=1) l = (
  (drawing())#set_foreground col;
  (drawing())#set_line_attributes ~width:thick ();
  List.iter draw_segment_taur l;

  (drawing())#set_foreground cl_fg;    
  (drawing())#set_line_attributes ~width:1 ();
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
    draw_segments ~col ~thick segs
)  


let draw_node ?(col=cl_fg) ?(target=false) pos = 
  draw_cross ~col ~target pos


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

  
  let hbox = GPack.hbox ~packing:(window())#add () in
  pkr := hbox#add;
  fixed := Some (GPack.fixed ~width:1200 ~height:900 ~packing:(hbox#add) ());

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
