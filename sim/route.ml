(* *** ********* *)
(* LER Simulator *)
(* *** ********* *)

(* Simple list-based representation of a LER route. *)


open Common
open Printf

type 'a hop = {hop:'a; anchor:'a; anchor_age: Common.time_t ; mutable searchcost:float}

type 'a t = 'a hop list

let create () = []


let add_hop path hop = path @ [hop]
  (* according to lib/ocaml/list.mli, non-tail recursion can be ignored when <
     10000 elements, which a route is almost certain be *)

let append_hops ~front ~back = front @ back

let nth_hop path n = List.nth path n
let last_hop path = Misc.listlast path

let length path = List.length path


let i2c routei = (
  List.map 
    (fun x -> 
      {hop=(Gworld.world())#nodepos x.hop;
      anchor=(Gworld.world())#nodepos x.anchor;
      anchor_age=x.anchor_age;
      searchcost=x.searchcost}) 
    routei
)


let route_valid route ~src ~dst= (

  let length() =  (
    ((length route >= 1) || raise (Failure "length not >= 1"))
    &&
    ((if length route = 1 then (src = dst) else true) || 
    raise (Failure "length 1, src <> dst"))
  ) in
  
  let start_at_src() = 
    ((nth_hop route 0).hop = src) || raise (Failure "Route does not start at src") 
  in
  
  let end_at_dst() = 
    ((nth_hop route (List.length route - 1)).hop = dst) || raise (Failure "Route does not end at dst")
  in
  

  (* searchcost non-zero only when anchors change *)    
  let searchcost() = (
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
    || raise (Failure "Searchcost was non-zero without anchor change")
  ) in

  (* anchor_age monotonically decreasing *)
  let anchor_age_decreasing() = (
    (let rec advance r last_age = (
      
      match r with 
	| [] -> true
	| hop::nexthops -> (
	    if hop.anchor_age > last_age then 
	      false 
	    else
	      advance nexthops hop.anchor_age
	  )
    )  in 
    advance route max_float)
    || raise (Failure "Anchor age not monotonically decreasing")
  ) in
  
  (* anchor_age can only change when anchor changes *)
  let anchor_age_changes_with_anchor() = (
    (let rec advance front back = (
      match (front, back) with 
	| ([], hopb::rb) -> (
	    (* first hop *)
	    (hopb.anchor_age <= max_float )
	    &&
	    advance [hopb] rb 
	  )
	| (hopf::rf, hopb::rb) -> (
	    (if (hopb.anchor_age <> hopf.anchor_age) then hopb.anchor <> hopf.anchor else true)
	    &&
	    advance (hopb::hopf::rf) rb
	  )
	| (rf, []) -> 
	    true
    ) in 
    advance [] route) 
    || raise (Failure "Anchor age changed but anchor did not")
  ) in

  (* loop-free (no hop is repeated twice)*)
  let loop_free() = (
    List.fold_left  
    (fun stat h ->
      ((List.length (List.filter (fun k -> k.hop = h.hop) route)) = 1) && stat)
    true 
    route
  || raise (Failure "Route has a loooop")
  )
  in    
  length()
  &&
  start_at_src()
  &&
  end_at_dst()
  &&
  searchcost()
  &&
  anchor_age_decreasing()
  &&
  anchor_age_changes_with_anchor()
  &&
  loop_free()
)
	  
let eucl_length ~dist_f route = (
  let rec recurse_ r len = 
    match r with 
      | [dst] -> len
      | a::b::c -> recurse_ (b::c) (len +. (dist_f a.hop b.hop))
      | [] -> raise (Misc.Impossible_Case "Route.eucl_length")
  in
recurse_ route 0.0
)

let anchor_cost route = 
  List.fold_left (fun cost hop -> cost +. (hop.searchcost)) 0.0 route

let sprint route = (
  String.concat 
  ("\n")
  (List.map
    (fun x -> Printf.sprintf "hop:%s \tanchor:%s \tage:%s \tcost:%f" 
      (Coord.sprintf x.hop) 
      (Coord.sprintf x.anchor)
      (sprintf "%2f" x.anchor_age)
      x.searchcost)
    route)
)  
  
