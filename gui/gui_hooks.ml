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





let route_done = ref false

let ease_route_pktin_mhook routeref l2pkt node = (

  let l3pkt = (L2pkt.l3pkt l2pkt) in
  let l3hdr = (L3pkt.l3hdr l3pkt) in
  let l3dst = L3pkt.l3dst l3pkt
  and l3src = L3pkt.l3src l3pkt in

  match (L2pkt.l2src l2pkt) <> node#id with
    | _ -> 	(* Packet arriving at a node *)
	(Log.log)#log_debug (lazy (Printf.sprintf "Arriving t node %d\n" node#id));	  
	
	if  node#id = l3dst then ( (* Packet arriving at dst. *)
	  route_done := true;
	  routeref := Route.add_hop !routeref {
	    Route.hop=node#pos;
	    Route.anchor=(L3pkt.l3anchor l3pkt);
	    Route.anchor_age=(L3pkt.l3enc_age l3pkt);
	    Route.searchcost=0.0; (* hack see general_todo.txt *)
	  }
	)
	  (* this should not be a failure. ie, a node can send a packet to itself, if 
	     it was closest to the previous anchor, searches for a new anchor, and is
	     closest to this anchor too *)
	  (*    | false ->  assert(false)*)
)

let ease_route_pktout_mhook routeref l2pkt node = (
  
  let l3pkt = (L2pkt.l3pkt l2pkt) in
  let l3hdr = (L3pkt.l3hdr l3pkt) in
  let l3dst = L3pkt.l3dst l3pkt 
  and l3src = L3pkt.l3src l3pkt in
  
  match (L2pkt.l2src l2pkt) <> node#id with
    | true -> 	assert(false)
    | false ->  (* Packet leaving some node *)
	
	(Log.log)#log_info (lazy (Printf.sprintf "Leaving src %d\n" node#id));	
	routeref := Route.add_hop !routeref {
	  Route.hop=node#pos;
	    Route.anchor=(L3pkt.l3anchor l3pkt);
	    Route.anchor_age=(L3pkt.l3enc_age l3pkt);
	    Route.searchcost=(L3pkt.l3search_dist l3pkt)
	}
)

let mtr_2_pix_route r = 
  List.map 
    (fun h -> 
      {h with
	Route.hop=(pos_mtr_to_pix h.Route.hop);
	Route.anchor=(pos_mtr_to_pix h.Route.anchor)
	}
    ) r
