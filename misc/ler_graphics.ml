(*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
(*                                                                                      *)
(*  Graphics                                                                            *)
(*                                                                                      *)
(*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

open Misc
open Coord
open Printf

let init_gfx () = (
  Graphics.open_graph " 600x600";
)

let close_gfx () = Graphics.close_graph ()
let clear_gfx () = Graphics.clear_graph ()

let scale_x x = f2i (x *. i2f (Graphics.size_x ()))
let scale_y y = f2i (y *. i2f (Graphics.size_y ()))

let unscale_x x = (i2f x) /. (i2f (Graphics.size_x ()))
let unscale_y y = (i2f y) /. (i2f (Graphics.size_y ()))

let scale_pos p = [|scale_x (x p); scale_y (y p)|]
let unscale_pos p = [|unscale_x (x p); unscale_y  (y p)|]

let scale_points l = Array.map (fun p -> scale_pos p)  l


let draw_nodes a =   begin
  let _drawnode n = 
    let c1 = x (scale_pos n)  
    and c2 = y (scale_pos n)  in
    let segments = [|(c1 - 2, c2, c1 + 2, c2) ; (c1, c2 - 2, c1, c2 + 2)|] in
      Graphics.draw_segments segments 
  in
    Array.iter _drawnode a
end

let label_nodes a =   begin
  let _labelnode i n = 
    let c1 = x (scale_pos n)  
    and c2 = y (scale_pos n)  in
      Graphics.moveto  (c1 + 3) (c2 + 3);
      Graphics.draw_string (string_of_int i) 
  in
    Array.iteri _labelnode a

end
 
let label_node pos label = begin
    let c1 = x (scale_pos pos)  
    and c2 = y (scale_pos pos)  in
    Graphics.moveto (c1 + 3) (c2 + 3);
    Graphics.draw_string label
end

let draw_and_label_nodes l = draw_nodes l; label_nodes l

let circle_nodes l radius = begin

  let sc_radius = scale_x radius in
  let scaled_points = scale_points l in
    Array.iter (fun p -> Graphics.draw_circle (x p) (y p) sc_radius) scaled_points
end

    


(* takes a segment s as [|(x1, y1); (x2, y2)|]  (normalized, ie all components in [0,1])
   and returns the complement within the bounds of the grid.
   ie, returns the two segments that join the extremities of s to the borders.
   Does not check if s touches a border, in which case the returned segment(s) may be a point*)

let complement_segment seg = (

  if ((Array.get seg 0) = (Array.get seg 1)) then 
    raise (Failure "intersect_segment_x: two points of segment are identical!");
  let x1 = x (Array.get seg 0) and y1 = y (Array.get seg 0) and
    x2 = x (Array.get seg 1) and y2 = y (Array.get seg 1) in
  let intersections = ref [] in
  (* find intersections with Ox and Oy, keep the one which is at the borders of our surface *)
  let lambda = (0.0 -. x1) /. (x1 -. x2) in
  let py_ll = y1 +. lambda *. (y1 -. y2) in
  let lambda = (0.0 -. y1) /. (y1 -. y2) in
  let px_ll = x1 +. lambda *. (x1 -. x2) in
  (* find intersections with right hand or upper border *)
  let lambda = (1.0 -. x1) /. (x1 -. x2) in
  let py_ur = y1 +. lambda *. (y1 -. y2) in
  let lambda = (1.0 -. y1) /. (y1 -. y2) in
  let px_ur = x1 +. lambda *. (x1 -. x2) in
  
  if (x1 = x2)  then begin (* vertical segment *)
    intersections := !intersections @ [[|x1; 0.0|]];
    intersections := !intersections @ [[|x1; 1.0|]];
  end  
  else if (y1 = y2) then begin (* horizontal segment *)
    intersections := !intersections @ [[|0.0;  y1|]];
    intersections := !intersections @ [[|1.0;  y1|]];
  end 
  else if (x1 -. x2) /. (y1 -. y2) = 1.0  then begin (* 'forward' diagonal segment *)
    intersections := !intersections @ [[|0.0;0.0|]];
    intersections := !intersections @ [[|1.0;1.0|]];
  end
  else if (x1 -. x2) /. (y1 -. y2) = -1.0 then begin (* 'forward' diagonal segment *)
    intersections := !intersections @ [[|0.0;1.0|]];
    intersections := !intersections @ [[|1.0;0.0|]];
  end
  else begin
    let on_border x = (x >= 0.0 && x <= 1.0) in
    if on_border px_ll then intersections := !intersections @ [[|px_ll; 0.0|]];
    if on_border py_ll then intersections := !intersections @ [[|0.0; py_ll|]];
    if on_border px_ur  then intersections := !intersections @ [[|px_ur; 1.0|]];
    if on_border py_ur then  intersections := !intersections @ [[|1.0; py_ur|]];
  end;
  
  (* xxx/henri assert fails with <0.05,0.05> <0.1,0.1> *)
  assert (List.length !intersections = 2);
  if (dist_sq (List.nth !intersections 0) [| x1; y1|]) < (dist_sq (List.nth !intersections 0) [| x2; y2|]) then 
    [|(List.nth !intersections 0); [|x1; y1|]; [|x2; y2|]; (List.nth !intersections 1) |]
  else 
    [| (List.nth !intersections 0); [|x2; y2|]; [|x1; y1|]; (List.nth !intersections 1) |]
)
  
(* 
   Take a coord (assumed 2D), and reflect it if it is out of the boundaries.
   This assumes that the point is at most within one grid 
   length out of the boundaries (otherwise we should do modulos)
*)
let reflect pos = (
  let newx = ref (x pos) and newy = ref (y pos) in 
    if !newx >  1.0 then 
      newx := !newx -. 1.0
    else if !newx < 0.0 then
      newx := 1.0 +. !newx;
    if !newy > 1.0  then  
      newy := !newy -. 1.0
    else if !newy < 0.0 then
      newy := 1.0 +. !newy;
    assert (!newx >= 0.0 && !newx <  1.0 && !newy >= 0.0 && !newy <  1.0);
    [|!newx; !newy|];
)

let center_and_reflect_nodes nodes point = (

  let delta = [|0.5;0.5|] ---. point in
  let offset_nodes = Array.map (fun p -> delta +++. p) nodes in
    Array.map reflect offset_nodes ;
)




(* takes two points as [|(x1, y1); (x2, y2)|]. (normalized, ie all components in [0,1])
   If the shortest path between points is direct, returns as is. 
   If the shortest path is via wrapping over the boundary, returns two segments showing the wrapping
*)
let reflect_segment seg = (

  if (Array.length seg) != 2 then 
    raise (Failure (Printf.sprintf "reflect_segment: got segment of length %d" (Array.length seg)));
  let c_and_r node refnode = 
    Array.get (center_and_reflect_nodes [|node|] refnode) 0 in
  let p1 = Array.get seg 0 and p2 = Array.get seg 1 in
  let d1 = dist_sq p1 p2 and d2 = dist_sq [|0.5;0.5|]  (c_and_r p1 p2) in

  if ( d1 <= d2 ) then
    seg
  else
    complement_segment seg
)      
      

  
let ler_draw_segment a = 
  assert (Array.length a == 2);
  let scaled = scale_points a in
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
      let pt =  (i2f i) *. ( (i2f n) /. (i2f (Graphics.size_x ())))  in
	ler_draw_segment [| [|pt; 0.0|]; [|pt; 1.0|]|];
	ler_draw_segment [| [|0.0; pt|]; [|1.0; pt|]|];
    end
    done
end

let draw_gradient gradient_matrix = (
  for i = 0 to (Array.length gradient_matrix - 1) do 
  for j = 0 to (Array.length gradient_matrix.(0) - 1) do 
      ler_draw_segment [|coord_i2f [|i; j|] ; (coord_i2f  [|i; j|]) +++. ((gradient_matrix.(i).(j)) ///. 2.0)|]
    done;
  done;
)

let draw_cross point w = (
  let quad_of_pairs p1 p2 = (x p1, y p1, x p2, y p2) in
  let wx = [|w; 0|] and wy = [|0; w|] in
  let p = scale_pos point in
    Graphics.draw_segments [| (quad_of_pairs (p --- wx)  (p +++ wx)); 
			      (quad_of_pairs (p --- wy)  (p +++ wy)); 
			   |];

)

let draw_route route = (
  let i = ref 0 in
  let rec recurse_ route = (
    let color_seq = [| Graphics.black; Graphics.red; Graphics.green;
    Graphics.blue; Graphics.magenta |] in

  match route with 
    | [] -> ()
    | hop1::hop2::r -> (
	Graphics.set_color color_seq.(!i mod (Array.length color_seq));
	circle_nodes [|hop1.Route.hop|] (hop1.Route.searchcost);
	ler_draw_segments [|hop1.Route.hop;hop2.Route.hop|];
	incr i;
	recurse_ (hop2::r);
      )
    | hop1::r -> ()
  )
  in recurse_ route
)




let mouse_choose_node get_nodes_at msg = (
  printf "%s \n" msg; flush Pervasives.stdout;

  let rec try_till_chosen () = (
    let s1 = Graphics.wait_next_event [Graphics.Button_down] in
    let n = get_nodes_at (unscale_pos [|s1.Graphics.mouse_x; s1.Graphics.mouse_y|]) in
      if n = [] then (
	printf "sorry, no node here\n" ; flush stdout;
	try_till_chosen ();
      ) else (
	label_node (unscale_pos [|s1.Graphics.mouse_x; s1.Graphics.mouse_y|]) (string_of_int (List.hd n));
	List.hd n
      ) 
  ) in 
    try_till_chosen ();
)




let dump_window outfile = (
  let colorarray = Graphics.dump_image (Graphics.get_image 0 0 (Graphics.size_x ()) (Graphics.size_y()))  in
  let fd = open_out_bin outfile in
  Marshal.to_channel fd colorarray [];
  close_out fd
)



