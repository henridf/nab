open Misc
open Set
open Hashtbl

module type GraphType = HashedType

module type S = 
sig
  type elt
  type g
  type graphtype_t = Directed | Undirected

  val create_ : elt -> int -> graphtype_t -> g

  val neigbors_  :  g -> elt -> elt list
  val neigborsi_ : g -> int -> int list

  val contains_  : g -> elt -> bool
  val containsi_ : g -> int -> bool

  val add_node_  : g -> elt -> unit
  val add_edge_  : g -> elt -> elt  -> float ->  unit
  val add_edgei_ : g -> int -> int  -> float -> unit

  val iteri_ : (int  -> unit) -> g -> unit
  val itern_ : (elt  -> unit) -> g -> unit
end;;



module Make(G: GraphType) = 
struct

  module Hash = Hashtbl.Make (G)

  type elt = G.t
  type cost_t = Nan | Cost of float     
  type adj_mat_t = cost_t array array   

  type graphtype_t = Directed | Undirected 
  type g = { mutable ind : int;  (* index of next new node in nodes array. Equal to size when full *)
	     size : int;         (* max # nodes in graph *)
	     nodes : elt array;   
	     hash : int Hash.t;
	     m : adj_mat_t;
	     t : graphtype_t;}


  let index__ g n = Hash.find g.hash n (* throws Not_found *)
(* (
    let rec _index i = (
      if i >= g.ind then raise Not_found;
      if g.nodes.(i) = n then i
      else _index (i + 1)
    )
    in _index 0
  )
*)

		    

  let create_ node size gtype = {ind = 0; 
				 size = size;
				 nodes = Array.create size node;
				 hash = Hash.create size;
				 m = Array.create_matrix size size Nan; 
				 t = gtype}

				  
  let iteri_ f g = for i = 0 to (g.ind - 1) do f i done
  let itern_ f g = for i = 0 to (g.ind - 1) do f g.nodes.(i) done


  let contains_ g n = try ignore (index__ g n); true with Not_found -> false
(*
  let contains_ g n  = (
    let rec _contains i = i < g.ind && ((g.nodes.(i) = n) or (_contains (i + 1))) in
      _contains 0
  )
*)

  let containsi_ g i = g.ind > i
			 
  let add_edge_ g n1 n2 c  = (
    if (n1 = n2) then raise (Invalid_argument "Graph.add_edge_: cannot connect a node to itself");
    try 
      let x = index__ g n1 and y = index__ g n2 in
 	g.m.(x).(y) <- Cost c;
	if g.t = Undirected then g.m.(y).(x) <- Cost c;
    with 
	Not_found -> raise (Invalid_argument  "Graph.add_edge_: node does not exist");
  )

  let add_edgei_ g i1 i2 c = (
    if (i1 = i2) then raise (Invalid_argument "Graph.add_edgei_: cannot connect a node to itself");
    if i1 >= g.ind or i2 >= g.ind then raise (Invalid_argument  "Graph.add_edgei_: index does not exist");
    g.m.(i1).(i2) <- Cost c;
    if g.t = Undirected then g.m.(i2).(i1) <- Cost c;
  )

  let add_node_ g n = (
    if g.ind = g.size then failwith "add_node: graph is full";
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
      let i = index__ g n in
	List.map (fun j -> g.nodes.(j)) (neigborsi_ g i)
    with
	Not_found -> raise (Invalid_argument "Graph.neigborsi_: node does not exist")
  )

end;;

module FloatGraphType = 
struct 
  type t = float
  let equal = (=)
  let hash = Hashtbl.hash
end;;

module IntGraphType = 
struct 
  type t = int
  let equal = (=)
  let hash = Hashtbl.hash
end;;

module FloatGraph = Make(FloatGraphType);;
module IntGraph = Make(IntGraphType);;

let test_ () = (
  let module FG = FloatGraph in 
    
  let g = FG.create_ 0.0 10 FG.Undirected in 
    assert (FG.contains_ g 0.0 = false);
    assert (FG.containsi_ g 0 = false);
    FG.add_node_ g 0.0;
    assert (FG.contains_ g 0.0 = true);
    assert (FG.containsi_ g 0 = true);
    assert (FG.neigbors_ g 0.0 = []);
    assert (FG.neigborsi_ g 0 = []);
    
    FG.add_node_ g 1.0;
    FG.add_edgei_ g 0 1 0.0;
    assert (FG.neigbors_ g 0.0 = [1.0]);
    assert (FG.neigbors_ g 1.0 = [0.0]);
    
    FG.add_node_ g 2.0;
    FG.add_node_ g 3.0;
    FG.add_edge_ g 2.0 3.0 0.0;
    assert (FG.neigborsi_ g 2 = [3]);
    assert (FG.neigborsi_ g 3 = [2]);
    
    FG.add_edge_ g 0.0 2.0 0.0;
    FG.add_edge_ g 0.0 3.0 0.0;
    FG.add_edge_ g 2.0 1.0 0.0;
    FG.add_edge_ g 3.0 1.0 0.0;
    
    let ngbrs = FG.neigborsi_ g 1 in 
      assert (List.length ngbrs = 3);
      assert (List.mem 0 ngbrs);
      assert (List.mem 2 ngbrs);
      assert (List.mem 3 ngbrs);

      let g = FG.create_ 0.0 10 FG.Directed in 
	FG.add_node_ g 0.0;
	FG.add_node_ g 1.0;
	FG.add_node_ g 2.0;
	FG.add_node_ g 3.0;
	
	FG.add_edgei_ g 0 1 0.0;
	assert (FG.neigborsi_ g 0 = [1]);
	assert (FG.neigborsi_ g 1 = []);

	Printf.printf "Graph.test_ : passed \n";
)   
		 
let _ = test_  ()		 










