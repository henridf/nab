(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)
open Misc

let x_pix_size = ref (Param.get Params.x_pix_size)
let y_pix_size = ref (Param.get Params.y_pix_size)

let init() = (
 x_pix_size :=  (Param.get Params.x_pix_size);
 y_pix_size :=  (Param.get Params.y_pix_size)
)

let x_mtr() = Param.get Params.x_size
and y_mtr() = Param.get Params.y_size

let x_mtr_to_pix x = f2i ((i2f !x_pix_size) *. (x /. x_mtr()))
let y_mtr_to_pix y = f2i ((i2f !y_pix_size) *. (y /. y_mtr()))

let x_pix_to_mtr x = (x_mtr()) *. ((i2f x) /. (i2f !x_pix_size))
let y_pix_to_mtr y = (y_mtr()) *. ((i2f y) /. (i2f !y_pix_size))


let pos_mtr_to_pix pos = 
  (x_mtr_to_pix (Coord.xx pos), y_mtr_to_pix (Coord.yy pos))

let pos_pix_to_mtr pos = 
  (x_pix_to_mtr (Coord.xx pos), y_pix_to_mtr (Coord.yy pos))


let closest_node_at pix_pos = 
  let pos = pos_pix_to_mtr pix_pos in
    (o2v ((Gworld.world())#find_closest ~pos ~f:(fun _ -> true)))

let mtr_2_pix_route r = 
  List.map 
    (fun h -> 
      {h with
	Route.hop=(pos_mtr_to_pix h.Route.hop);
	Route.anchor=(pos_mtr_to_pix h.Route.anchor)
	}
    ) r
