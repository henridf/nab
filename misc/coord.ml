open Misc
type 'a coord_t = ('a * 'a)
type coordi_t = int coord_t
type coordf_t = float coord_t
type coordn_t = int array

let (+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let (+++.) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)

let (---) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let (---.) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)


let ( *** ) (x, y) scalar = (x * scalar, y * scalar)
let ( ***. ) (x, y) scalar = (x *. scalar, y *. scalar)

let ( /// ) (x, y) scalar = (x / scalar, y / scalar)
let ( ///. ) (x, y) scalar = (x /. scalar, y /. scalar)


let coord_i2f (x, y) = (i2f x, i2f y)
let coord_i2n (x, y) = [| float x; float y|]
let coord_f2i (x, y) = (f2i x, f2i y)
let coord_f2n (x, y) = [|x, y|]
let coord_round (x, y) = (round x, round y)
let coord_floor (x, y) = (floor x, floor y)

let xx (x, y) = x
let yy (x, y) = y


let normi_sq (x, y) = 
  (powi ~num:x ~exp:2) + (powi ~num:y ~exp:2)
    
let normi c = 
  sqrt (i2f (normi_sq c))

let disti_sq  (x1, y1) (x2, y2) = 
  (powi ~num:(x1 - x2) ~exp:2) + (powi ~num:(y1 - y2) ~exp:2)

let disti c1 c2 = (
  sqrt (i2f (disti_sq c1 c2))
)

let norm_sq (x, y) =  (x ** 2.0) +. (y ** 2.0)
let norm c = sqrt (norm_sq c)

let dist_sq  (x1, y1) (x2, y2) = 
  ((x1 -. x2) ** 2.0) +. ((y1 -. y2) ** 2.0)

let dist c1 c2 = (
  sqrt  (dist_sq c1 c2)
)

let normalize p = 
  let n = norm p in
  match n with 
    | 0.0 -> p
    | norm -> p ///. norm

let print (x, y) = 
  Printf.printf "<%d, %d>" x y

let sprint (x, y) =   Printf.sprintf "<%d, %d>" x y

let sprintf (x, y) = Printf.sprintf  "<%.3f, %.3f>" x y
