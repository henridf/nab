type 'a t = Empty | Node of 'a * 'a t list

let rec belongs el =  function
  | Empty -> false
  | Node (v, t) -> (v = el) || (List.exists (belongs el) t)

let rec height = 
  let max_list l = List.fold_left max 0 l in
  function 
    | Empty -> 0
    | Node (_, t) -> 1 + (max_list (List.map height t))

let rec iter f = 
  function
    | Empty -> ()
    | Node (el, t) -> f el; List.iter (iter f) t

