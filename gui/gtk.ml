(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Coord
open Misc
open GMain

let cl_fg = `NAME "blue" 
let cl_bg = `BLACK


let x_gtk_scale = ref 750
let y_gtk_scale = ref 750

let x_mws_scale = ref 0.0
let y_mws_scale = ref 0.0

let x_mws_to_gtk x = f2i ((i2f !x_gtk_scale) *. (x /. !x_mws_scale))
let y_mws_to_gtk y = f2i ((i2f !y_gtk_scale) *. (y /. !y_mws_scale))
let pos_mws_to_gtk pos = (x_mws_to_gtk (xx pos), y_mws_to_gtk (yy pos))

let set_mws_scale ~x ~y = (
  x_mws_scale := x;
  y_mws_scale := y
)

let init, window, drawing = (
  let wnd = ref None in
  let drw = ref None in
  let window_ () = o2v !wnd in
  let drawing_ () = o2v !drw in
  let init_ () = (
    wnd := Some (GWindow.window ~show:true ());
    let area = GMisc.drawing_area ~width:!x_gtk_scale ~height:!y_gtk_scale ~packing:(window_())#add () in
    drw := Some (area#misc#realize (); new GDraw.drawable (area#misc#window));
    (drawing_())#set_foreground cl_fg;
    (drawing_())#set_background cl_bg;
  ) in
  (init_, window_, drawing_)
)


(* pixcenter in gtk pixels *)
let draw_cross ~pixcenter = 
  let pixwid = 3 in
  let x, y = pixcenter in
  (drawing())#segments  [
    (x, y - pixwid), (x, y + pixwid);
    (x - pixwid, y), (x + pixwid, y)
  ]

let erase draw_action = (
  (drawing())#set_foreground cl_bg;
  draw_action();
  (drawing())#set_foreground cl_fg;
)

let draw_node ~mwspos = (
  let gtkpos = pos_mws_to_gtk mwspos in
  draw_cross ~pixcenter:gtkpos
)

let undraw_node ~mwspos = erase (fun _ -> draw_node ~mwspos:mwspos)
  
    
