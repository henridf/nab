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

  val create_ : elt -> int -> graphtype_t -> t

  val neigbors_  : t -> elt -> elt list
  val neigborsi_ : t -> int -> int list
  val nhop_neigbors_  : t -> node:elt -> radius:int -> elt list
  val nhop_neigborsi_ : t -> index:int -> radius:int -> int list

  val size_      : t -> int          (* number of nodes in graph, might be <> than max. size of graph *)
  val contains_  : t -> elt -> bool
  val containsi_ : t -> int -> bool

  val setinfo_   : t -> elt -> int list -> unit
  val setinfoi_  : t -> int -> int list -> unit
  val getinfo_   : t -> elt -> int list
  val getinfoi_  : t -> int -> int list

  val add_node_  : t -> elt -> unit
  val add_edge_  : t -> elt -> elt  -> float ->  unit
  val add_edgei_ : t -> int -> int  -> float -> unit

  val iteri_ : (int  -> unit) -> t -> unit
  val itern_ : (elt  -> unit) -> t -> unit
  val print_ : t -> unit
end;;



module Make(G: GraphType) = 
struct

  module Hash = Hashtbl.Make (G)

  type elt = G.t
  type cost_t = Nan | Cost of float     
  type adj_mat_t = cost_t array array   

  type graphtype_t = Directed | Undirected 
  type t = { mutable ind : int;  (* index of next new node in nodes array. Equal to size when full *)
	     size : int;         (* max # nodes in graph *)
	     nodes : elt array;   
	     info : int list array; (* allows client to store info in the graph, like which walkers are at a node *)
	     hash : int Hash.t;
	     m : adj_mat_t;
	     t : graphtype_t
	   }


  let index__ g n = Hash.find g.hash n (* throws Not_found *)
  
  let create_ node size gtype = {ind = 0; 
				 size = size;
				 nodes =  Array.create size node;
				 info = Array.create size [];
				 hash =  Hash.create size;
				 m =  Array.create_matrix size size Nan;
				 t = gtype}
				  
				  
  let iteri_ f g = for i = 0 to (g.ind - 1) do f i done
  let itern_ f g = for i = 0 to (g.ind - 1) do f g.nodes.(i) done


  let size_ g = g.ind

  let contains_ g n = try ignore (index__ g n); true with Not_found -> false
  let containsi_ g i = g.ind > i
			 
  let getinfoi_ g i = (
    if i >= g.ind then raise (Invalid_argument "Graph.getinfoi_ : index does not exist");
    g.info.(i);
  )

  let getinfo_ g n  = (
    try 
      let i = index__ g n in 
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
      let i = index__ g n in 
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
      let x = index__ g n1 and y = index__ g n2 in 
	add_edgei_ g x y c
    with 
	Not_found -> raise (Invalid_argument  "Graph.add_edge_: node does not exist");
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
	Not_found -> raise (Invalid_argument "Graph.neigbors_: node does not exist")
  )

  let neigborsi_list__ g l = (
    let res = ref [] in
    let ngbrs = List.flatten (List.map (fun i -> neigborsi_ g i) l) in
      List.iter (fun i -> if not (List.mem i !res ) then res := i::!res) ngbrs;
      !res
  )


  let nhop_neigborsi_ g ~index ~radius = (
    if not (containsi_ g index) then raise (Invalid_argument "Graph.nhop_neigborsi_: index does not exist");

    let seen_yet = Array.create (size_ g) false in
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
      let i = index__ g node in
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
    
    assert (list_same (FG.neigborsi_list__ g [0; 1; 2; 3]) [0; 1; 2; 3]);

    (* fully connected
       0 -> 1 2 3 
       1 -> 0 2 3 
       2 -> 0 1 3 
       3 -> 0 1 2 
    *)
    let ngbrs = FG.neigborsi_ g 1 
    and ngbrs_1hop = FG.nhop_neigborsi_ g ~index:1 ~radius:1 
    and ngbrs_0hop = FG.nhop_neigborsi_ g ~index:1 ~radius:0 
    and ngbrs_2hop = FG.nhop_neigborsi_ g ~index:1 ~radius:2 in 
      assert (List.length ngbrs = 3);
      assert (list_same ngbrs [0; 2; 3]);
      assert (list_same ngbrs ngbrs_1hop);
      assert (ngbrs_2hop = []);
      assert (ngbrs_0hop = [1]);

      let g = FG.create_ 0.0 10 FG.Directed in 
	FG.add_node_ g 0.0;
	FG.add_node_ g 1.0;
	FG.add_node_ g 2.0;
	FG.add_node_ g 3.0;
	
	FG.add_edgei_ g 0 1 0.0;
	assert (FG.neigborsi_ g 0 = [1]);
	assert (FG.neigborsi_ g 1 = []);

	assert ((FG.getinfo_ g 0.0) = []);
	assert ((FG.getinfoi_ g 0) = []);
	FG.setinfo_ g 0.0 [0;0];
	FG.setinfoi_ g 1 [1;1];
	assert ((FG.getinfo_ g 0.0) = [0;0]);
	assert ((FG.getinfoi_ g 0) = [0;0]);
	assert ((FG.getinfo_ g 1.0) = [1;1]);
	assert ((FG.getinfoi_ g 1) = [1;1]);

	Printf.printf "Graph.test_ : passed \n";
)   
		 
let _ = test_  ()		 










