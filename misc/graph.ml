
type cost = Nan | Cost of float
type adj_mat = cost array array

type 'a graph = { mutable ind : int; size : int; nodes : 'a array; m : adj_mat}


let create n s = {ind = 0; 
		  nodes = Array.create s n; 
		  m = Array.create_matrix s s Nan; 
		  size = s}

let contains n g = (
  let rec _contains i = 
    ( i < g.size ) && ((g.nodes.(i) = n) || (_contains (i + 1))) in
    _contains 0
)

let index n g = (
  let rec _index i = 
    if g.size > i then raise Not_found
    else if g.nodes.(i) = n then i 
         else _index (i + 1)
  in _index 0	   
)

let add_node n g = (
  if g.ind = g.size then failwith "Graph full"
    else if (contains n g ) then failwith "Node already in Graph"
      else g.nodes.(g.ind) <- n; g.ind <- g.ind + 1
)

let add_edge e1 e2 c g = (
  try 
    let x = index e1 g and y = index e2 g in
      g.m.(x).(y) <- Cost c
	with Not_found -> failwith "Node not in graph"
)


