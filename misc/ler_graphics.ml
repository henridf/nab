(*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
(*                                                                                      *)
(*  Graphics                                                                            *)
(*                                                                                      *)
(*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

open Misc
open Itin
open Lattice
open Ler_utils
open Printf


let init_gfx () = (
  Graphics.open_graph " 600x600";
)

let close_gfx () = Graphics.close_graph ()
let clear_gfx () = Graphics.clear_graph ()

let scale_x x = 
  (* recompute scales each time for testcases where gridsize is changed *)
  let xratio = float_of_int (Graphics.size_x ()) /. (i2f params.gridsize) in
  f2i (round (xratio *. x))

let scale_y y = 
  let yratio = float_of_int (Graphics.size_y ()) /. (i2f params.gridsize) in
  f2i (round (yratio *. y))

let unscale_x x = 
  let xratio = float_of_int (Graphics.size_x ()) /. (i2f params.gridsize) in
  f2i (round (x /. xratio))
  
let unscale_y y = 
  let yratio = float_of_int (Graphics.size_y ()) /. (i2f params.gridsize) in
  f2i (round (y /. yratio))

let scale_pos p = (scale_x (i2f (x p)),  scale_y (i2f (y p)))
let unscale_pos p = (unscale_x (i2f (x p)),  unscale_y (i2f (y p)))
let scale_posf p = (scale_x  (x p),  scale_y  (y p))
let unscale_posf p = (unscale_x  (x p),  unscale_y  (y p))

let scale_points l = Array.map (fun p -> scale_pos p)  l
let scale_pointsf l = Array.map (fun p -> scale_posf p)  l


let draw_nodes a =   begin
  let _drawnode n = 
    let (c1, c2) = scale_pos n  in
    let segments = [|(c1 - 2, c2, c1 + 2, c2) ; (c1, c2 - 2, c1, c2 + 2)|] in
      Graphics.draw_segments segments 
  in
    Array.iter _drawnode a
end

let label_nodes a =   begin
  let _labelnode i n = 
    let (c1, c2) = scale_pos n  in
      Graphics.moveto  (c1 + 3) (c2 + 3);
      Graphics.draw_string (string_of_int i) 
  in
    Array.iteri _labelnode a

end
 
let label_node pos label = begin
  let (xs, ys) = scale_pos pos in
    Graphics.moveto (xs + 3) (ys + 3);
    Graphics.draw_string label
end

let draw_and_label_nodes l = draw_nodes l; label_nodes l

let circle_nodes l radius = begin

  let sc_radius = scale_x radius in
  let scaled_points = scale_points l in
    Array.iter (fun (x, y) -> Graphics.draw_circle x y sc_radius) scaled_points
end

    


(* takes an segment s as [|(x1, y1); (x2, y2)|] and returns the complement within the bounds of the grid.
   ie, returns the two segments that join the extremities of s to the borders.
   Does not check if s touches a border, in which case the returned segment(s) may be a point*)
let complement_segment seg = begin 

  if ((Array.get seg 0) = (Array.get seg 1)) then 
    raise (Failure "intersect_segment_x: two points of segment are identical!");
  let x1 = i2f (x (Array.get seg 0)) and y1 = i2f (y  (Array.get seg 0)) and
    x2 = i2f (x (Array.get seg 1)) and y2 = i2f (y  (Array.get seg 1)) in
  let intersections = ref [] in
    (* find intersections with Ox and Oy, keep the one which is at the borders of our surface *)
  let lambda = (0.0 -. x1) /. (x1 -. x2) in
  let py_ll = y1 +. lambda *. (y1 -. y2) in
  let lambda = (0.0 -. y1) /. (y1 -. y2) in
  let px_ll = x1 +. lambda *. (x1 -. x2) in
    (* find intersections with right hand or upper border *)
  let lambda = (i2f params.gridsize -. x1) /. (x1 -. x2) in
  let py_ur = y1 +. lambda *. (y1 -. y2) in
  let lambda = (i2f params.gridsize -. y1) /. (y1 -. y2) in
  let px_ur = x1 +. lambda *. (x1 -. x2) in
    
    if (x1 = x2)  then begin
      intersections := !intersections @ [(f2i x1, 0)];
      intersections := !intersections @ [(f2i x1, params.gridsize)];
    end  
    else if (y1 = y2) then begin
      intersections := !intersections @ [(0, f2i y1)];
      intersections := !intersections @ [(params.gridsize, f2i y1)];
    end 
    else begin
      (* 2nd & 4th inequalities are  sharp so that if our segment touches the corner (ie, at (0.0, 0.0))
	 we don't count both points needlessly. Same for the 2nd py_ur inequality *)
      let on_border x = (x >= 0.0 && x <= (i2f params.gridsize)) in
      let on_border_sharp x = (x > 0.0 && x < (i2f params.gridsize)) in
      if on_border px_ll then intersections := !intersections @ [(f2i px_ll, 0)];
      if on_border_sharp py_ll then intersections := !intersections @ [(0, f2i py_ll)];
      if on_border px_ur  then intersections := !intersections @ [(f2i px_ur, params.gridsize)];
      if on_border_sharp py_ur then  intersections := !intersections @ [(params.gridsize, f2i py_ur)];
    end;


    List.iter (fun (x, y) -> printf "%d %d\n" x y) !intersections;
    assert (List.length !intersections = 2);
    if (dist_sq (List.nth !intersections 0) (f2i x1, f2i y1)) < (dist_sq (List.nth !intersections 0) (f2i x2, f2i y2)) then 
      [|(List.nth !intersections 0); (f2i x1, f2i y1); (f2i x2, f2i y2); (List.nth !intersections 1) |]
    else 
      [| (List.nth !intersections 0); (f2i x2, f2i y2); (f2i x1, f2i y1); (List.nth !intersections 1) |]
   
end


(* takes two points as [|(x1, y1); (x2, y2)|]. 
   If the shortest path between points is direct, returns as is. 
   If the shortest path is via wrapping over the boundary, returns two segments showing the wrapping
*)

let reflect_segment seg = begin

  if (Array.length seg) != 2 then 
    raise (Failure (Printf.sprintf "reflect_segment: got segment of length %d" (Array.length seg)));
  let c_and_r node refnode = 
    Array.get (center_and_reflect_nodes (ref [|node|]) refnode) 0 in
  let p1 = Array.get seg 0 and p2 = Array.get seg 1 in
  let d1 = dist_sq p1 p2 and d2 = dist_sq params.center  (c_and_r p1 p2) in

    if ( d1 <= d2 ) then
      seg
    else
      complement_segment seg

end
      
      

  
let ler_draw_segment a = 
  assert (Array.length a == 2);
  let scaled = scale_points a in
    Graphics.draw_segments [| x scaled.(0), y scaled.(0), x scaled.(1), y scaled.(1)|]

let ler_draw_segmentf a = 
  assert (Array.length a == 2);
  let scaled = scale_pointsf a in
    Graphics.draw_segments [| x scaled.(0), y scaled.(0), x scaled.(1), y scaled.(1)|]

(* takes a list of points and connects them *)
let ler_draw_segments a = (
  for i = 0 to (Array.length a) - 2 do 
    ler_draw_segment ([|a.(i); a.(i+1)|]);
  done;
)

(* takes a list of points, reflects and connects them *)
let ler_draw_segments_reflect a = 
  for i = 0 to (Array.length a) - 2 do 
    let segments = reflect_segment [|a.(i); a.(i+1)|] in
    ler_draw_segment ([|segments.(0); segments.(1)|]);
      if (Array.length segments == 4) then ler_draw_segment ([|segments.(2); segments.(3)|]);
  done

let draw_grid n = begin
  for i = 0 to n do
    begin
      let pt =  (i * params.gridsize) /  n in
	ler_draw_segment [| (pt, 0); (pt, params.gridsize)|];
	ler_draw_segment [| (0, pt); (params.gridsize, pt)|];
    end
    done
end

let draw_gradient gradient_matrix = (
  for i = 0 to (params.gridsize - 1) do 
    for j = 0 to (params.gridsize - 1) do
      ler_draw_segmentf [|pair_i2f (i, j) ; (pair_i2f  (i, j)) +++. ((gradient_matrix.(i).(j)) ///. 2.0)|]
    done;
  done;
)

let draw_cross point w = (
  let quad_of_pairs (a, b) (c, d) = (a, b, c, d) in
  let wx = (w, 0) and wy = (0, w) in
  let p = scale_pos point in
    Graphics.draw_segments [| (quad_of_pairs (p --- wx)  (p +++ wx)); 
			      (quad_of_pairs (p --- wy)  (p +++ wy)); 
			   |];

)

let animate_route route f = (
  if (Array.length route <> 0) then
  let start = ref (route.(Array.length route - 1)) in
    for i = (Array.length route - 2) downto 0 do
      let next = route.(i) in
	ler_draw_segments_reflect [|!start; next|];
	f (scale_pos  next);
	start := next;
    done;
)

let draw_route route  = animate_route route (fun x -> ())

let animate_itin itin lattice f = 
  let route = Array.map (
    fun i ->
      let tuple_as_array =  Lattice.node_ lattice i in
	(tuple_as_array.(0), tuple_as_array.(1))
  ) (Itinerary.toarray_ itin) in
    animate_route route f

let draw_itin itin lattice = animate_itin itin lattice (fun x -> ())

(*
  let mouse_choose_node msg = (
  printf "%s \n" msg; flush Pervasives.stdout;

  let rec try_till_chosen () = (
    let s1 = Graphics.wait_next_event [Graphics.Button_down] in
    let n = Ler.get_node_at (unscale_pos (s1.Graphics.mouse_x, s1.Graphics.mouse_y)) in
      if n = [] then (
	printf "sorry, no node here\n" ; flush stdout;
	try_till_chosen ();
      ) else (
	label_node (unscale_pos (s1.Graphics.mouse_x, s1.Graphics.mouse_y)) (string_of_int (List.hd n));
	List.hd n
      ) 
  ) in 
    try_till_chosen ();
)
*)



let dump_window outfile = (
  let colorarray = Graphics.dump_image (Graphics.get_image 0 0 (Graphics.size_x ()) (Graphics.size_y()))  in
  let fd = open_out_bin outfile in
  Marshal.to_channel fd colorarray [];
  close_out fd
)



