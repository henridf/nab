(* Open things:
   t.info is hardwired to be int lists. Maybe should be flexible (like nodes are type elt)
*)
open Misc
open Set
open Hashtbl

module type GraphType = HashedType

module type S = 
sig
  type elt
  type t
  type graphtype_t = Directed | Undirected

  val make_ : elt -> int -> graphtype_t -> t

  val neigbors_  : t -> elt -> elt list
  val neigborsi_ : t -> int -> int list
  val nhop_neigbors_  : t -> node:elt -> radius:int -> elt list
  val nhop_neigborsi_ : t -> index:int -> radius:int -> int list

  val size_      : t -> int          (* number of nodes in graph, might be <> than max. size of graph *)
  val index_     : t -> elt -> int
  val node_      : t -> int -> elt
  val contains_  : t -> elt -> bool
  val containsi_ : t -> int -> bool
    

  val setinfo_   : t -> elt -> int list -> unit (* convenience functions to allow client to associate 'info' with each node *)
  val setinfoi_  : t -> int -> int list -> unit (* Graph module blindly manipulates 'info' *)
  val getinfo_   : t -> elt -> int list         
  val getinfoi_  : t -> int -> int list
  val getinfosi_ : t -> int list -> int list    

  val add_node_  : t -> elt -> unit
  val add_edge_  : t -> elt -> elt  -> float ->  unit
  val add_edgei_ : t -> int -> int  -> float -> unit

  val route_dij_  : t -> src:elt -> dest:elt -> elt list (* all of these can raise (Failure "No_route") *)
  val routei_dij_ : t -> src:int -> dest:int -> int list
  val dist_       : t -> src:elt -> dest:elt -> int
  val disti_      : t -> src:int -> dest:int -> int

  val iteri_ : (int  -> unit) -> t -> unit
  val itern_ : (elt  -> unit) -> t -> unit
  val print_ : t -> unit
end;;



module Make(G: GraphType) : S with type elt = G.t = 
struct

  module Hash = Hashtbl.Make (G) 

  type elt = G.t
  type cost_t = Nan | Cost of float     
  type adj_mat_t = cost_t array array   

  type graphtype_t = Directed | Undirected 
  type t = { mutable ind : int;  (* index of next new node in nodes array. Equal to maxsize when full *)
	     maxsize : int;         (* max # nodes in graph *)
	     nodes : elt array;   
	     info : int list array; (* allows client to store info in the graph, like which walkers are at a node *)
	     hash : int Hash.t;
	     m : adj_mat_t;
	     t : graphtype_t
	   }


  let index_ g n = Hash.find g.hash n (* throws Not_found *)
  let node_ g i = g.nodes.(i)
  
  let make_ node size gtype = {ind = 0; 
			       maxsize = size;
			       nodes =  Array.make size node;
			       info = Array.make size [];
			       hash =  Hash.create size;
			       m =  Array.make_matrix size size Nan;
			       t = gtype}
				  
				  
  let iteri_ f g = for i = 0 to (g.ind - 1) do f i done
  let itern_ f g = for i = 0 to (g.ind - 1) do f g.nodes.(i) done


  let size_ g = g.ind

  let contains_ g n = try ignore (index_ g n); true with Not_found -> false
  let containsi_ g i = g.ind > i
			 
  let getinfoi_ g i = (
    if i >= g.ind then raise (Invalid_argument "Graph.getinfoi_ : index does not exist");
    g.info.(i);
  )

  let getinfosi_ g l = (
    List.flatten (
      List.map (fun i -> 
		  if i >= g.ind then raise (Invalid_argument "Graph.getinfosi_ : index does not exist");
		  g.info.(i);
	       ) l
    )
  )

  let getinfo_ g n  = (
    try 
      let i = index_ g n in 
	getinfoi_ g i 
    with
	Not_found -> raise (Invalid_argument  "Graph.getinfo_: node does not exist");
  )

  let setinfoi_ g i info = (
    if i >= g.ind then raise (Invalid_argument "Graph.setinfoi_ : index does not exist");
    g.info.(i) <- info
  )

  let setinfo_ g n info = (
    try 
      let i = index_ g n in 
	setinfoi_ g i info
    with
	Not_found -> raise (Invalid_argument  "Graph.setinfo_: node does not exist");
  )

  let add_edgei_ g i1 i2 c = (
    if (i1 = i2) then raise (Invalid_argument "Graph.add_edgei_: cannot connect a node to itself");
    if i1 >= g.ind or i2 >= g.ind then raise (Invalid_argument  "Graph.add_edgei_: index does not exist");
    g.m.(i1).(i2) <- Cost c;
    if g.t = Undirected then g.m.(i2).(i1) <- Cost c;
  )

  let add_edge_ g n1 n2 c  = (
    if (n1 = n2) then raise (Invalid_argument "Graph.add_edge_: cannot connect a node to itself");
    try 
      let x = index_ g n1 and y = index_ g n2 in 
	add_edgei_ g x y c
    with 
	Not_found -> raise (Invalid_argument  "Graph.add_edge_: node does not exist");
  )


  let add_node_ g n = (
    if g.ind = g.maxsize then failwith "add_node: graph is full";
    if contains_ g n then raise (Invalid_argument "Graph.add_node_: node already exists")
    else (
      g.nodes.(g.ind) <- n; 
      Hash.add g.hash n g.ind;
      g.ind <- g.ind + 1;
    )
  )

  let neigborsi_ g index = (
    (* xxx/slow : 3 allocations *)
    if containsi_ g index then 
      List.filter (fun n -> n >= 0) (Array.to_list (Array.mapi (fun i c -> if c <> Nan then i else -1) g.m.(index)))
    else
      raise (Invalid_argument "Graph.neigborsi_: index does not exist")
  )

  let neigbors_ g n = (
    try 
      let i = index_ g n in
	List.map (fun j -> g.nodes.(j)) (neigborsi_ g i)
    with
	Not_found -> raise (Invalid_argument "Graph.neigbors_: node does not exist")
  )

  let neigborsi_list__ g l = (
    let res = ref [] in
    let ngbrs = List.flatten (List.map (fun i -> neigborsi_ g i) l) in
      List.iter (fun i -> if not (List.mem i !res ) then res := i::!res) ngbrs;
      !res
  )

  let a_cost c = match c with Nan -> false | _ -> true
  let float_of_cost c = match c with Nan -> failwith "float_of_cost" | Cost x -> x

  exception Found of int

  let add_cost c1 c2 = match (c1, c2) with
      Cost x, Cost y -> Cost (x +. y)
    | Nan, Cost y -> c2
    | Cost x, Nan -> c1
    | Nan, Nan -> c1

  let less_cost c1 c2 = match (c1, c2) with
      Cost x, Cost y -> x < y
    | Cost x, Nan -> true
    | _, _ -> false
	
		     
  exception No_way

  let routei_dij_ g ~src ~dest = (

    let paths = Array.create (size_ g) (-1)
    and already_treated = Array.create (size_ g) false
    and distances = Array.create (size_ g) Nan
    and nn = (size_ g)
    and source = src
    in
      
    let first_not_treated () = (
      try 
	for i = 0 to nn-1 do if not already_treated.(i) then raise (Found i) done;
	raise Not_found;
      with Found i -> i
    ) in
    

    let least_not_treated p  = (
      let index = ref p 
      and dist = ref distances.(p) in
	for i = p+1 to nn-1 do
	  if not already_treated.(i) then 
	    if less_cost distances.(i) !dist then (
	      dist := distances.(i);
	      index := i
	    )
	done;
	!index, !dist;
    ) in
				   
    let one_round g = (
      let p = first_not_treated () in
      let np, nc = least_not_treated p in
	if not(a_cost nc) then raise No_way
	else (
	  already_treated.(np) <- true;
	  for i = 0 to nn-1 do
	    if not already_treated.(i) then
	      if a_cost g.m.(np).(i) then 
		let ic = add_cost distances.(np) g.m.(np).(i) in
		  if less_cost ic distances.(i) then (
		    paths.(i) <- np;
		    distances.(i) <- ic;
		  )
	  done;
	)
    ) in

			   
    let dij_ g src = (
      if containsi_ g src then ( 
	  
	  for j=0 to (size_ g - 1) do
	    let c = g.m.(src).(j) in 
	      distances.(j) <- c;
	      if a_cost c then paths.(j) <- src;
	  done;
	  try
	    for k = 0 to nn-2 do ignore (one_round g) done;
	  with No_way -> ()
      )
      else failwith "dij : unknown node";
      
    ) in
      if src = dest then [] else (
	already_treated.(src) <- true;	
	dij_ g src;
	let rec makepath i route = 
	  if paths.(i) =  src then route else   (makepath paths.(i) route) @ [paths.(i)]
	in
	  if not (a_cost distances.(dest)) then raise (Failure "No_route") else
	    (makepath dest []) @ [dest]
      )
  )      
				   
  let route_dij_ g ~src ~dest = (
    let routei = routei_dij_ g (index_ g src) (index_ g dest) in
      List.map (fun x -> node_ g x) routei
  )
				  
  let dist_ g ~src ~dest = List.length (route_dij_ g src dest)
  let disti_ g ~src ~dest = List.length (routei_dij_ g src dest)
    

  let nhop_neigborsi_ g ~index ~radius = (
    if not (containsi_ g index) then raise (Invalid_argument "Graph.nhop_neigborsi_: index does not exist");

    let seen_yet = Array.make (size_ g) false in
    let curngbrs = ref [index] in
      seen_yet.(index) <- true;
      let step n = (
	let candidates = neigborsi_list__ g n in
	let next_hop_ngbrs = ref [] in
	  List.iter (fun n -> 
		       if not (seen_yet.(n)) then (
		       next_hop_ngbrs := n::!next_hop_ngbrs;
		       seen_yet.(n) <- true;
		     )
		     else ()
		  ) candidates;
	!next_hop_ngbrs;
    )
    in
      for i = 1 to radius do
	curngbrs := (step !curngbrs);
      done;
      !curngbrs
  )

  let nhop_neigbors_ g ~node ~radius = (
    try 
      let i = index_ g node in
	List.map (fun j -> g.nodes.(j)) (nhop_neigborsi_ g ~index:i ~radius:radius)
    with
	Not_found -> raise (Invalid_argument "Graph.nhop_neigbors_: node does not exist")
  )
				   
  let print_ g = (
    iteri_ (fun i -> 
	      let ngbrs = neigborsi_ g i in
		Printf.printf "%d -> " i;
		List.iter (fun n -> Printf.printf "%d " n) ngbrs;
		Printf.printf "\n"
	   ) g
  )
end;;









