open Misc
type 'a coord_t = 'a array
type coordi_t = int coord_t
type coordf_t = float coord_t


let (+++) (c1:coordi_t) (c2:coordi_t) = Array.mapi (fun i v -> v + c2.(i)) c1
let (+++.) (c1:coordf_t) (c2:coordf_t) = Array.mapi (fun i v -> v +. c2.(i)) c1

let (---) (c1:coordi_t) (c2:coordi_t) = Array.mapi (fun i v -> v - c2.(i)) c1
let (---.) c1 c2 = Array.mapi (fun i v -> v -. c2.(i)) c1

let ( *** ) (c1:coordi_t) scalar = Array.mapi (fun i v -> v * scalar) c1
let ( ***. ) (c1:coordf_t) scalar = Array.mapi (fun i v -> v *. scalar) c1

let (///) (c1:coordi_t) scalar = Array.mapi (fun i v -> v / scalar) c1
let (///.) (c1:coordf_t) scalar = Array.mapi (fun i v -> v /. scalar) c1

let coordi2pair c = (c.(0), c.(1))
let coordf2pair c = (c.(0), c.(1))

let coord_i2f (c1:coordi_t) = Array.map (fun x -> i2f x) c1
let coord_f2i (c1:coordf_t) = Array.map (fun x -> f2i x) c1
let coord_round (c1:coordf_t) = Array.map (fun x -> round x) c1
let coord_floor (c1:coordf_t) = Array.map (fun x -> floor x) c1

let x c = c.(0) 
let y c = c.(1)


let normi_sq (c:coordi_t) = 
  let res = ref 0 in Array.iter (fun x -> res := !res + (powi ~num:x ~exp:2)) c; 
  !res
    
let normi (c:coordi_t) = 
  sqrt (i2f (normi_sq c))

let disti_sq c1 c2 = (
  let res = ref 0 in 
  Array.iteri (
    fun i x -> res := !res + (powi (x - c2.(i)) 2)
  ) c1 ;
  !res
)

let disti c1 c2 = (
  sqrt (i2f (disti_sq c1 c2))
)

let norm_sq (c:coordf_t) = 
  let res = ref 0.0 in Array.iter (fun x -> res := !res +. (x ** 2.0)) c; 
  !res

let norm (c:coordf_t) = 
  sqrt (norm_sq c)

let dist_sq c1 c2 = (
  let res = ref 0.0 in 
  Array.iteri (
    fun i x -> res := !res +. ((x -. c2.(i)) ** 2.0)
  ) c1 ;
  !res
)

let dist c1 c2 = (
  sqrt  (dist_sq c1 c2)
)

let normalize p = 
  let n = norm p in
  match n with 
    | 0.0 -> p
    | norm -> p ///. norm

let print p = (
  Printf.printf "<"; 
  Array.iter (fun x -> Printf.printf "%d  " x) p;
  Printf.printf ">"; 
  flush stdout
)

let sprint p = (
  Printf.sprintf "<" ^
  (Array.fold_left (fun s x -> s ^ (Printf.sprintf "%d " x) ) "" p) ^
  Printf.sprintf ">"; 
)

let sprintf p = (
  Printf.sprintf "<" ^
  (Array.fold_left (fun s x -> s ^ (Printf.sprintf "%.2f " x) ) "" p) ^
  Printf.sprintf ">"; 
)
