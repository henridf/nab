open Coord
open Misc


(* general remarks:
   1. After making any mods, check that it is working by uncommenting the test
   code at the end of Crworld.find_closest.
   2. Can still optimize. 
   - in neighboring_squares, could probably only take one of the two (say
     always take the upper one), and avoid the list_unique_elements at the end.
     xxx/for some reasone this is quite tricky. spent an hour on it, couldn't
     work.

   - instead of iterating over all lines, of grid, could prune and only
     iterate over those which are within one radius of the center.
   - in Crworld.get_nodes_in_ring:
     we effectively compute each ring twice 
   - *list_unique_elements* + fold_left + filter = expensive!!
   3. there is unnecessary duplication of code (advanceup vs advanceright,
   xsect_circle_hline vs xsect_circle_vline 
*)

(* check if x is in World.and not nan (can happen in sqrt computation of circle) *)
let exists_and_in_world x worldsize = 
  if (x >= 0.0 && x < worldsize) then [x]
  else []

(* Intersects the (center, radius) circle with a horizontal line at y, 
   and returns the list of 0, 1, or 2 intersection points depending on their 
   existence and their being within the World.*)
   
let xsect_circle_hline ~center ~radius ~y ~worldsize_x = (
  let rsq = radius ** 2.0 in
  let c = center in
  let x1 = exists_and_in_world ((xx c) +. sqrt (rsq -. (y -. (yy c))**2.0)) worldsize_x
  and x2 = exists_and_in_world ((xx c) -. sqrt (rsq -. (y -. (yy c))**2.0)) worldsize_x
  in x1 @ x2
)

(* Intersects the (center, radius) circle with a vertical line at x, 
   and returns the list of 0, 1, or 2 intersection points depending on their 
   existence and their being within the World.*)
let xsect_circle_vline ~center ~radius ~x ~worldsize_y = (
  let rsq = radius ** 2.0 in
  let c = center in
  let y1 = exists_and_in_world ((yy c) +. sqrt (rsq -. (x -. (xx c))**2.0)) worldsize_y
  and y2 = exists_and_in_world ((yy c) -. sqrt (rsq -. (x -. (xx c))**2.0)) worldsize_y
  in y1 @ y2
)



let xsect_grid_and_circle 
  ~center 
  ~radius 
  ~worldsize_x
  ~worldsize_y 
  ~boxsize = (
    
    let pos_in_grid pos = coord_f2i (coord_floor (pos ///. boxsize)) in
    
    let xmin = max 0.0 (floor ((xx center) -. radius))
    and xmax = min worldsize_x (floor ((xx center) +. radius))
    and ymin = max 0.0 (floor ((yy center) -. radius))
    and ymax = min worldsize_y (floor ((yy center) +. radius)) in
    let rec advanceup_ y l = 
      if (y > ymax) then 
	l
      else 
	let neighboring_squares x = 
	  match y with
	    | 0.0 -> [pos_in_grid (x, y)]
	    | y  -> 
		[pos_in_grid (x, y);
		(pos_in_grid (x, y)) +++ (0, -1)]
	in
	let pts = List.fold_left 
	  (fun l x -> l @ neighboring_squares x)
	  []
	  (xsect_circle_hline ~center ~radius ~y ~worldsize_x) 
	in
	advanceup_ (y +. boxsize) pts@l
    in
    let rec advanceright_ x l = 
      if (x > xmax) then 
	l 
      else 
	let neighboring_squares y = 
	  match x with
	    | 0.0 -> [pos_in_grid (x, y)]
	    | x -> [pos_in_grid (x, y);
	      (pos_in_grid (x, y)) +++ (-1, 0)]

	in
	let pts = List.fold_left
	  (fun l y ->  l @ neighboring_squares y)
	  []
	  (xsect_circle_vline ~center ~radius ~x ~worldsize_y) 
	in 
	advanceright_ (x +. boxsize) pts@l
    in
    let xpoints =  (advanceup_ ymin []) @ (advanceright_ xmin []) in 
    Misc.list_unique_elements xpoints
      
  )



(*     
used for visualizing in a toplevel
Ler_graphics.init_gfx();;
Ler_graphics.clear_gfx(); Ler_graphics.draw_grid 75;;

let color_seq = [| Graphics.red; Graphics.green;
Graphics.blue; Graphics.magenta |] ;;
let color_index = ref 0 ;;

let radius = ref 2.0;;
let next_circle() = Array.of_list (xsect_grid_and_circle ~center:(35.1, 35.1)
       ~radius:!radius ~worldsize:75.0) ;;
let scaleup p = p *** 10;;

let draw_rects a = (
  Graphics.set_color color_seq.(!color_index mod (Array.length color_seq));
  Array.iter (fun p -> Graphics.fill_rect (xx (scaleup p)) (yy (scaleup p))  10 10) a;
  Graphics.set_color Graphics.black;
  Ler_graphics.draw_grid 75;
  incr color_index
);;

let draw_current_circle () = (
  Graphics.set_color Graphics.black;
  Graphics.draw_circle 351 351 (f2i (!radius *. 10.0))
);;
*)
