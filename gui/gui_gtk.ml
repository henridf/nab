(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Coord
open Misc
open GMain

let cl_fg = `NAME "blue" 
let cl_bg = `BLACK


let x_pix_size = ref 1200
let y_pix_size = ref 900

let x_mtr_size = ref 1200.0
let y_mtr_size = ref 900.0

let x_mtr_to_pix x = f2i ((i2f !x_pix_size) *. (x /. !x_mtr_size))
let y_mtr_to_pix y = f2i ((i2f !y_pix_size) *. (y /. !y_mtr_size))
let pos_mtr_to_pix pos = (x_mtr_to_pix (xx pos), y_mtr_to_pix (yy pos))

let set_mtr_size ~x ~y = (
  x_mtr_size := x;
  y_mtr_size := y
)
(*
let wnd = ref None 
let drw = ref None 
let window () = o2v !wnd 
let drawing () = o2v !drw
*)
let init, window, drawing = (
  let wnd = ref None in
  let drw = ref None in
  let window_ () = o2v !wnd in
  let drawing_ () = o2v !drw in
  let init_ () = (
    wnd := Some (GWindow.window ~show:true ());
    let area = GMisc.drawing_area ~width:!x_pix_size ~height:!y_pix_size ~packing:(window_())#add () in
    drw := Some (area#misc#realize (); new GDraw.drawable (area#misc#window));
    (drawing_())#set_foreground cl_fg;
    (drawing_())#set_background cl_bg;
  ) in
  (init_, window_, drawing_)
)


(* pixcenter in gtk pixels *)
let draw_cross ~centerpix = 
  let pixwid = 3 in
  let x, y = centerpix in
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
  let gtkpos = pos_mtr_to_pix mwspos in
  draw_cross ~centerpix:gtkpos
)

let undraw_node ~mwspos = erase (fun _ -> draw_node ~mwspos:mwspos)
  
    
