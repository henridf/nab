(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Simple list-based representation of a LER route. *)
   
   


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

  (* length >= 1 *)
  (length route >= 1)
  &&
  (if length route = 1 then (src = dest) else true)
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
	  
let eucl_length ~dist_f route = (
  let rec recurse_ r len = 
    match r with 
      | [dst] -> len
      | a::b::c -> recurse_ (b::c) (len +. (dist_f a.hop b.hop))
      | [] -> raise Misc.Impossible_Case
  in
recurse_ route 0.0
)

let anchor_cost route = 
  List.fold_left (fun cost hop -> cost +. hop.searchcost) 0.0 route

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
  
