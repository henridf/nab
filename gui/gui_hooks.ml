(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc

let x_pix_size = ref 1200
let y_pix_size = ref 900

let x_mtr() = Param.get Params.x_size
and y_mtr() = Param.get Params.y_size

let x_mtr_to_pix x = f2i ((i2f !x_pix_size) *. (x /. x_mtr()))
let y_mtr_to_pix y = f2i ((i2f !y_pix_size) *. (y /. y_mtr()))
let pos_mtr_to_pix pos = 
  (x_mtr_to_pix (Coord.xx pos), y_mtr_to_pix (Coord.yy pos))


let node_moved newpos node = (
  let newpos_pix = pos_mtr_to_pix newpos in
  Gui_pos.enter_node_pos (node#id, newpos_pix)
)

let attach_mob_hooks()  = 
  Nodes.iter
    (fun n -> n#add_mob_mhook ~hook:node_moved)
