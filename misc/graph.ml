open Misc

(* Types and Exceptions *)

type cost_t = Nan | Cost of float     (* private *)
type adj_mat_t = cost_t array array   (* private *)



type graphtype_t = Directed | Undirected (* public *)
type 'a graph_t = { mutable ind : int;   (* public *)
		  size : int; 
		  nodes : 'a array;
		  m : adj_mat_t;
		  t : graphtype_t;}

(* Private Functions *)
let index n g = (
  let rec _index i = (
    if i >= g.size then raise Not_found;
    if g.nodes.(i) = n then i
    else _index (i + 1)
  )
  in _index 0
)

  
(* Public Functions *)

let create node size gtype = {ind = 0; 
			      size = size;
			      nodes = Array.create size node;
			      m = Array.create_matrix size size Nan; 
			      t = gtype}

			       
let iteri f g = for i = 0 to (g.ind - 1) do f i done
let itern f g = for i = 0 to (g.ind - 1) do f g.nodes.(i) done


let contains n g = (
  let rec _contains i = i < g.size && ((g.nodes.(i) = n) or (_contains (i + 1))) in
    _contains 0
)

let containsi i g = g.ind > i
 

let add_edge n1 n2 c g = (
  try 
    let x = index n1 g and y = index n2 g in
 	g.m.(x).(y) <- Cost c;
	if g.t = Undirected then g.m.(x).(y) <- Cost c;
  with 
      Not_found -> failwith "add_edge: node does not exist"
)

let add_edgei i1 i2 c g = (
  if i1 >= g.size or i2 >= g.size then failwith "add_edgei: node does not exist";
  g.m.(i1).(i2) <- Cost c;
  if g.t = Undirected then g.m.(i1).(i2) <- Cost c;
)


let add_node n g = (
  if g.ind = g.size then failwith "add_node: graph is full";
  if contains n g then failwith "add_node: node already exists"
  else (g.nodes.(g.ind) <- n; g.ind <- g.ind + 1)
)

let neigborsi i g = (
  if containsi i g then 
    List.filter (fun n -> n >= 0) (Array.to_list (Array.mapi (fun i c -> if c <> Nan then i else -1) g.m.(i)))
  else
    failwith "neigborsi: node does not exist"
)

let neigbors n g = (
  try 
    let i = index n g in
      List.map (fun j -> g.nodes.(j)) (neigborsi i g)
  with
    Not_found -> failwith "neigborsi: node does not exist"
)

(* Lattices *)

(* Returns the d-tuple of coordinates (represented as int list of length d) 
   for the indexth point in a d-dimensional hypercube of side length s *)
let tuple ~side ~dim ~index = (
  let ith i = (
    let rec _div j num = if j = 0 then num else (_div (j - 1) num/side) in
      (_div i index) mod side
  ) in
    Array.init dim ith
)

(* "wrap" a tuple around the borders of hypercube of side s. 
   Assumes that coordinates of the tuple are at out of boundaries by at most "s"
   (otherwise we should do modulos 
*)
let wrap_tuple s t = (
  Array.map (
    fun coord -> 
      if coord >= s then coord - s 
      else if coord < 0 then s + coord else coord
  ) t
)

let lattice_neigbors point side = (
  let _twoneigbors i = (
    let n1 = Array.copy point and n2 = Array.copy point in
      n1.(i) <- n1.(i) + 1;
      n2.(i) <- n2.(i) - 1;
      List.map (wrap_tuple side) [n1; n2]
  ) in
  let rec _allneigbors i  = 
    if i = (Array.length point) then []
    else (_twoneigbors i) @ _allneigbors (i + 1) 
  in
    _allneigbors 0
)

let create_lattice ~dim ~size = (
  if not (ispower ~pow:dim ~num:size) then failwith "create_lattice : incompatible dimension and size";
  let side = f2i ((i2f size) ** (1.0 /. i2f dim)) in
  
  let g = create (Array.create dim 0) size Undirected in
    g.ind <- g.size;

    (* fill in the nodes labels with their dim-dimensional coordinates *)
    iteri (fun index -> g.nodes.(index) <- tuple ~side:side ~dim:dim ~index:index) g;

    (* for each node, compute its neigbors and connect them *)
    itern (
      fun node -> 
	let ngbrs = lattice_neigbors node side in
	  List.iter (fun ngbr -> add_edge ngbr node 1.0 g) ngbrs
    ) g;

    g;
)



				
 










