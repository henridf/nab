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
let gdk_wnd = ref None 
let (drw : [ `window] GDraw.drawable option ref) = ref None 
let pkr = ref (fun _ -> ())
let window () = o2v !wnd 
let gdk_window () = o2v !gdk_wnd 
let drawing () = o2v !drw
let packer() =  !pkr
let fixed = ref None
let fix() = o2v !fixed
let txt_label = ref None
let txt() = o2v !txt_label

let q = Queue.create() 
let redraw _ = (
  (drawing())#set_foreground (`RGB (max_int, 0, 0));
(*  (drawing())#set_line_attributes ~width:10 ();

   (drawing())#polygon ~filled:false
    [ 10,100; 35,35; 100,10; 165,35; 190,100;
      165,165; 100,190; 35,165; 10,100 ];
*)

(*
n  if (not (Queue.is_empty q)) then (
 let n1 = Queue.pop q in
  (drawing())#set_line_attributes ~width:10 ();
  (drawing())#set_foreground (`WHITE);
  (drawing())#segments [n1];

  );
*)
  let l = 
   Queue.fold (fun l el -> el::l) [] q in
  if (List.length l > 0) then (
    (drawing())#set_line_attributes ~width:10 ();
    (drawing())#segments l;
  );
  true;
)

let clear() = 
  Gdk.Window.clear (gdk_window())

let init () = (
  wnd := Some (GWindow.window ~show:true ());

  let _ = (window())#connect#destroy ~callback:Main.quit in

  
  let vbox = GPack.vbox ~packing:(window())#add () in
  pkr := vbox#add;
  fixed := Some (GPack.fixed ~width:1200 ~height:900 ~packing:(vbox#add) ());

  gdk_wnd := Some ((fix())#misc#realize (); (fix())#misc#window) ;
  drw := Some (new GDraw.drawable (gdk_window()));
  let (pixmap, bitmap) = Gdk.Pixmap.create_from_xpm
    ~file:"/tmp/epfl.xpm" 
    ~window:(gdk_window())
    ()
  in
  Gdk.Window.set_back_pixmap (fix())#misc#window (`PIXMAP pixmap);
  
  txt_label := Some (GMisc.label ~text:"Go on!" ~packing:(packer()) ());
  ignore ((window())#event#connect#expose ~callback:redraw);
(*  ignore (Timeout.add ~ms:100 ~callback:redraw)*)

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
(*  (window())#misc#draw None;
  (drawing())#misc#draw None;*)
  (drawing())#segments s;

(*
  List.iter (fun seg -> 
    Queue.push seg q ) s
    ~v:(fun _ ->    *)


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
    (drawing())#set_foreground cl_hilite
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
    
