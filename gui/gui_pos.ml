(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc

let nodes_pos_ = ref [||]
let nodes_pos () = 
  if (!nodes_pos_ = [||]) then 
    nodes_pos_ := Array.make (Param.get Params.nodes) (0, 0);
  !nodes_pos_

let node_pos_inst nid t = 
  (t, (nodes_pos()).(nid))

let nodes_pos_inst nid_arr t = 
  Array.map (fun nid -> node_pos_inst nid t) nid_arr

let node_pos_range nid_arr ~start ~finish = 
  raise Not_Implemented

let nodes_pos_range nid_arr ~start ~finish = 
  Array.map (fun nid -> node_pos_range nid ~start ~finish) nid_arr


let enter_node_pos (nid, pos) = 
  (nodes_pos()).(nid) <- pos

let enter_nodes_pos arr = 
  Array.iter (fun (nid, pos) -> enter_node_pos (nid, pos)) arr
