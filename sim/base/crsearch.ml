(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Header$ *)



open Coord
open Pervasives
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
let exists_and_in_world x world_size = 
  if (x >= 0.0 && x < world_size) then [x]
  else []

(* Intersects the (center, radius) circle with a horizontal line at y, 
   and returns the list of 0, 1, or 2 intersection points depending on their 
   existence and their being within the World.*)
   
let xsect_circle_hline ~center ~radius ~y ~world_size_x = (
  let rsq = radius ** 2.0 in
  let c = center in
  let x1 = exists_and_in_world ((xx c) +. sqrt (rsq -. (y -. (yy c))**2.0)) world_size_x
  and x2 = exists_and_in_world ((xx c) -. sqrt (rsq -. (y -. (yy c))**2.0)) world_size_x
  in x1 @ x2
)

(* Intersects the (center, radius) circle with a vertical line at x, 
   and returns the list of 0, 1, or 2 intersection points depending on their 
   existence and their being within the World.*)
let xsect_circle_vline ~center ~radius ~x ~world_size_y = (
  let rsq = radius ** 2.0 in
  let c = center in
  let y1 = exists_and_in_world ((yy c) +. sqrt (rsq -. (x -. (xx c))**2.0)) world_size_y
  and y2 = exists_and_in_world ((yy c) -. sqrt (rsq -. (x -. (xx c))**2.0)) world_size_y
  in y1 @ y2
)


let grid_size_x, grid_size_y = 0, 0
	

let xsect_grid_and_circle 
  ~center 
  ~radius 
  ~world_size_x
  ~world_size_y 
  ~tile_size_x
  ~tile_size_y
  ~grid_size_x
  ~grid_size_y
  = (
    
    (*       Note: replicated from crworld.ml*)
    let pos_in_grid (x, y) = (
      assert (x >= 0. && y >= 0. && x <= world_size_x && y <= world_size_y);
      (* the 'min' is in case a point lies on the up/right boundary, in which case
	 the x /. tile_size_x_ division "fits" and we would get a grid_pos = to 
	 grid_size_x_ (resp. grid_size_y_). *)
      let x_pos = 
	min (grid_size_x - 1) (Misc.f2i (floor (x /. tile_size_x)))
      and y_pos =
	min (grid_size_y - 1) (Misc.f2i (floor (y /. tile_size_y)))
      in (x_pos, y_pos)
    ) in
    
    let xmin = max 0.0 (floor ((xx center) -. radius))
    and xmax = min world_size_x (floor ((xx center) +. radius))
    and ymin = max 0.0 (floor ((yy center) -. radius))
    and ymax = min world_size_y (floor ((yy center) +. radius)) in
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
	  (xsect_circle_hline ~center ~radius ~y ~world_size_x) 
	in
	advanceup_ (y +. tile_size_y) pts@l
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
	  (xsect_circle_vline ~center ~radius ~x ~world_size_y) 
	in 
	advanceright_ (x +. tile_size_x) pts@l
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
       ~radius:!radius ~world_size:75.0) ;;
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
