open Common

type 'a hop = {hop:'a; anchor:'a; searchcost:float}

type 'a t = 'a hop list

let create () = []


let add_hop path hop = path @ [hop]
  (* according to lib/ocaml/list.mli, non-tail recursion can be ignored when <
     10000 elements, which a route is almost certain be *)

let append_hops ~front ~back = front @ back

let nth_hop path n = List.nth path n

let length path = List.length path

let i2c data routei = (
  List.map 
    (fun x -> 
      {hop=data.pos.(x.hop); 
      anchor=data.pos.(x.anchor);
      searchcost=x.searchcost}) 
    routei
)

let route_valid route ~src ~dest= (

  (* length at least 2 *)
  (length route > 1)
  &&
(*  (Printf.printf "length OK\n"; true)
  &&*)
  (* start at source *)
  ((nth_hop route 0).hop = src)
  &&
(*    (Printf.printf "1st hop ok\n"; true) *)
(*    && *)
  (* end at dest *)
  ((nth_hop route (length route - 1)).hop = dest)
  &&
(*    (Printf.printf "last hop ok\n"; true) *)
(*    && *)
  (* searchcost non-zero only when anchors change *)
  (let rec advance front back = (

    match (front, back) with 
      | ([], hopb::rb) -> (
	  (* first hop: anchor search can be 0 or not, correct either way *)
	  if hopb.searchcost < 0.0 then (
	    false 
	  )
	  else 
	    advance [hopb] rb 
	)
      | (hopf::rf, hopb::rb) -> (
	  (hopb.searchcost >= 0.0)
	  &&
	  (if (hopb.searchcost > 0.0) then hopb.anchor <> hopf.anchor else true)
	  &&
	  advance (hopb::hopf::rf) rb
	)
      | (rf, []) -> 
	  true
  ) in 
  advance [] route)
)
	  
let sprint route = (
  String.concat 
  ("\n")
  (List.map
    (fun x -> Printf.sprintf "hop:%s \tanchor:%s \tcost:%f" 
      (Coord.sprintf x.hop) 
      (Coord.sprintf x.anchor)
      x.searchcost)
    route)
)  
  
