(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

val init : unit -> unit

(* use this function to pack things into the main vbox *)
val packer : 
  unit -> 
  GObj.widget -> 
  unit


(* use this function to set up a call back for 
   button_press event in the main vbox (bitmap) *)
val install_button_press_cb :  
  (GdkEvent.Button.t -> bool) ->   
  GtkSignal.id

val remove_button_press_cb :  GtkSignal.id -> unit

(* use this function to write a txt message in msg area *)
val txt_msg : string  -> unit

val redraw : 
  'a -> bool

val clear : 
  unit -> unit

val draw_node :  
  bool ->          (* if true then node is "hilited" *)
  Coord.coordi_t -> 
  unit

val draw_nodes : 
  Coord.coordi_t list 
  -> unit

val draw_segments : 
  (Coord.coordi_t * Coord.coordi_t)  list 
  -> unit

val draw_circle : 
  centr:Coord.coordi_t -> 
  radius:int
  -> unit
