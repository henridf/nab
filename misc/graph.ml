open Misc

module type Graph_t = 
sig
  type 'a graph_t
  type graphtype_t = Directed | Undirected

  val create_ : 'a -> int -> graphtype_t -> 'a graph_t
  val create_lattice_ : dim:int -> size:int -> int array graph_t

  val neigbors_  :  'a graph_t -> 'a -> 'a list
  val neigborsi_ : 'a graph_t -> int -> int list

  val contains_  : 'a graph_t -> 'a -> bool
  val containsi_ : 'a graph_t -> int -> bool

  val add_edge_  : 'a graph_t -> 'a -> 'a  -> float ->  unit
  val add_edgei_ : 'a graph_t -> int -> int  -> float -> unit

  val iteri_ : (int  -> unit) -> 'a graph_t -> unit
  val itern_ : ('a  -> unit) -> 'a graph_t -> unit
end;;


module Graph : Graph_t = 
struct

  (* Types and Exceptions *)

  type cost_t = Nan | Cost of float     
  type adj_mat_t = cost_t array array   

  type graphtype_t = Directed | Undirected 
  type 'a graph_t = { mutable ind : int;  (* index of next new node in nodes array. Equal to size when full *)
		      size : int;         (* max # nodes in graph *)
		      nodes : 'a array;   
		      m : adj_mat_t;
		      t : graphtype_t;}


  let index__ g n = (
    let rec _index i = (
      if i >= g.ind then raise Not_found;
      if g.nodes.(i) = n then i
      else _index (i + 1)
    )
    in _index 0
  )

		    

  let create_ node size gtype = {ind = 0; 
				 size = size;
				 nodes = Array.create size node;
				 m = Array.create_matrix size size Nan; 
				 t = gtype}

				  
  let iteri_ f g = for i = 0 to (g.ind - 1) do f i done
  let itern_ f g = for i = 0 to (g.ind - 1) do f g.nodes.(i) done


  let contains_ g n  = (
    let rec _contains i = i < g.size && ((g.nodes.(i) = n) or (_contains (i + 1))) in
      _contains 0
  )

  let containsi_ g i = g.ind > i
			 

  let add_edge_ g n1 n2 c  = (
    try 
      let x = index__ g n1 and y = index__ g n2 in
 	g.m.(x).(y) <- Cost c;
	if g.t = Undirected then g.m.(x).(y) <- Cost c;
    with 
	Not_found -> raise (Invalid_argument  "Graph.add_edge_: node does not exist");
  )

  let add_edgei_ g i1 i2 c = (
    if i1 >= g.ind or i2 >= g.ind then raise (Invalid_argument  "Graph.add_edgei_: index does not exist");
    g.m.(i1).(i2) <- Cost c;
    if g.t = Undirected then g.m.(i1).(i2) <- Cost c;
  )


  let add_node_ g n = (
    if g.ind = g.size then failwith "add_node: graph is full";
    if contains_ g n then raise (Invalid_argument "Graph.add_node_: node already exists")
    else (g.nodes.(g.ind) <- n; g.ind <- g.ind + 1)
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
      let i = index__ g n in
	List.map (fun j -> g.nodes.(j)) (neigborsi_ g i)
    with
	Not_found -> raise (Invalid_argument "Graph.neigborsi_: node does not exist")
  )

  (*
   *
   * Lattices 
   *
  *)

  (* val tuple : side:int -> dim:int -> index:int -> int array 
     Returns the d-tuple of coordinates (represented as int array of length d) 
     for the indexth point in a d-dimensional hypercube of side length s *)
  let tuple__ ~side ~dim ~index = (
    let ith i = (
      let rec _div j num = if j = 0 then num else (_div (j - 1) num/side) in
	(_div i index) mod side
    ) in
      Array.init dim ith
  )

  (* val wrap_tuple : int -> int array -> int array 
     Wrap a tuple around the borders of hypercube of side s. 
     Assumes that coordinates of the tuple are at out of boundaries by at most "s"
     (otherwise we should do modulos ) *)
  let wrap_tuple__ s t = (
    Array.map (
      fun coord -> 
	if coord >= s then coord - s 
	else if coord < 0 then s + coord else coord
    ) t
  )

  (* val lattice_neigbors__ : int array -> int -> int array list *)
  let lattice_neigbors__ point side = (

    let _twoneigbors i = (
      (* return the two neigbors along the ith axis *)
      let n1 = Array.copy point and n2 = Array.copy point in
 	n1.(i) <- n1.(i) + 1;
	n2.(i) <- n2.(i) - 1;
	List.map (wrap_tuple__ side) [n1; n2]
    ) in
    let rec _allneigbors i  = 
      if i = (Array.length point) then []
      else (_twoneigbors i) @ _allneigbors (i + 1) 
    in
      Gc.minor () ;
      _allneigbors 0
  )

  let create_lattice_ ~dim ~size = (
    if not (ispower ~pow:dim ~num:size) then raise (Invalid_argument "Graph.create_lattice_ : incompatible dimension and size");
    let side = f2i ((i2f size) ** (1.0 /. i2f dim)) in
      
    let g = create_ (Array.create dim 0) size Undirected in
      g.ind <- g.size;

      (* fill in the nodes labels with their dim-dimensional coordinates *)
      iteri_ (fun index -> g.nodes.(index) <- tuple__ ~side:side ~dim:dim ~index:index) g;

      (* for each node, compute its neigbors and connect them *)
      itern_ (
	fun node -> 
	  let ngbrs = lattice_neigbors__ node side in
	    List.iter (fun ngbr -> add_edge_ g ngbr node 1.0) ngbrs
      ) g;

      g;
  )

end;;













