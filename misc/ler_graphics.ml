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

(* $Header *)



(*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
(*                                                                                      *)
(*  Graphics                                                                            *)
(*                                                                                      *)
(*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

open Misc
open Coord
open Printf

let gfx_open = ref false

let init_gfx () = (
  if not !gfx_open then (
    Graphics.open_graph " 600x600";
    gfx_open := true
  )
)

let close_gfx () = (
  if !gfx_open then (
  Graphics.close_graph ();
    gfx_open := false 
  )
)

let clear_gfx () = Graphics.clear_graph ()

let scale_x x = f2i (x *. i2f (Graphics.size_x ()))
let scale_y y = f2i (y *. i2f (Graphics.size_y ()))

let unscale_x x = (i2f x) /. (i2f (Graphics.size_x ()))
let unscale_y y = (i2f y) /. (i2f (Graphics.size_y ()))

let scale_pos (x, y) = (scale_x x, scale_y y)
let unscale_pos (x, y) = (unscale_x x, unscale_y y)

let scale_points l = Array.map (fun p -> scale_pos p)  l


let draw_nodes a =   begin

  let _drawnode n = 
    if (Array.length a <=1000)then (
    let c1 = xx (scale_pos n)  
    and c2 = yy (scale_pos n)  in
    let segments = [|(c1 - 1,  c2, c1 + 1, c2) ; (c1, c2 - 1, c1, c2 + 1)|] in
      Graphics.draw_segments segments 
    ) else (
    let c1 = xx (scale_pos n)  
    and c2 = yy (scale_pos n)  in
    let intensity = Random.int 120 in
    Graphics.set_color (Graphics.rgb intensity intensity intensity);
    let segments = [|(c1, c2, c1, c2) |] in
      Graphics.draw_segments segments 
    ) 
      
  in
    Array.iter _drawnode a
end

let label_nodes a =   begin
  let _labelnode i n = 
    let c1 = xx (scale_pos n)  
    and c2 = yy (scale_pos n)  in
      Graphics.moveto  (c1 + 3) (c2 + 3);
      Graphics.draw_string (string_of_int i) 
  in
    Array.iteri _labelnode a

end
 
let label_node pos label = begin
    let c1 = xx (scale_pos pos)  
    and c2 = yy (scale_pos pos)  in
    Graphics.moveto (c1 + 3) (c2 + 3);
    Graphics.draw_string label
end

let draw_and_label_nodes l = draw_nodes l; label_nodes l

let circle_nodes ?(fill=false) l radius = begin
  let circle_f = if fill then Graphics.fill_circle else Graphics.draw_circle in

  let sc_radius = if fill then (scale_x radius) - 1 else (scale_x radius) in
  let scaled_points = scale_points l in
    Array.iter (fun p -> circle_f (xx p) (yy p) sc_radius) scaled_points
end

    


(* takes a segment s as [|(x1, y1); (x2, y2)|]  (normalized, ie all components in [0,1])
   and returns the complement within the bounds of the grid.
   ie, returns the two segments that join the extremities of s to the borders.
   Does not check if s touches a border, in which case the returned segment(s) may be a point*)

let complement_segment seg = (

  if ((Array.get seg 0) = (Array.get seg 1)) then 
    raise (Failure "intersect_segment_x: two points of segment are identical!");
  let x1 = xx (Array.get seg 0) and y1 = yy (Array.get seg 0) and
    x2 = xx (Array.get seg 1) and y2 = yy (Array.get seg 1) in
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
    intersections := !intersections @ [(x1, 0.0)];
    intersections := !intersections @ [(x1, 1.0)];
  end  
  else if (y1 = y2) then begin (* horizontal segment *)
    intersections := !intersections @ [(0.0,  y1)];
    intersections := !intersections @ [(1.0,  y1)];
  end 
  else if (x1 -. x2) /. (y1 -. y2) = 1.0  then begin (* 'forward' diagonal segment *)
    intersections := !intersections @ [(0.0,0.0)];
    intersections := !intersections @ [(1.0,1.0)];
  end
  else if (x1 -. x2) /. (y1 -. y2) = -1.0 then begin (* 'forward' diagonal segment *)
    intersections := !intersections @ [(0.0,1.0)];
    intersections := !intersections @ [(1.0,0.0)];
  end
  else begin
    let on_border x = (x >= 0.0 && x <= 1.0) in
    if on_border px_ll then intersections := !intersections @ [(px_ll, 0.0)];
    if on_border py_ll then intersections := !intersections @ [(0.0, py_ll)];
    if on_border px_ur  then intersections := !intersections @ [(px_ur, 1.0)];
    if on_border py_ur then  intersections := !intersections @ [(1.0, py_ur)];
  end;
  
  (* xxx/henri assert fails with <0.05,0.05> <0.1,0.1> *)
  assert (List.length !intersections = 2);
  if (dist_sq (List.nth !intersections 0) ( x1, y1)) < (dist_sq (List.nth !intersections 0) ( x2, y2)) then 
    [|(List.nth !intersections 0); (x1, y1); (x2, y2); (List.nth !intersections 1) |]
  else 
    [| (List.nth !intersections 0); (x2, y2); (x1, y1); (List.nth !intersections 1) |]
)
  
(* 
   Take a coord (assumed 2D), and reflect it if it is out of the boundaries.
   This assumes that the point is at most within one grid 
   length out of the boundaries (otherwise we should do modulos)
*)
let reflect pos = (
  let newx = ref (xx pos) and newy = ref (yy pos) in 
    if !newx >  1.0 then 
      newx := !newx -. 1.0
    else if !newx < 0.0 then
      newx := 1.0 +. !newx;
    if !newy > 1.0  then  
      newy := !newy -. 1.0
    else if !newy < 0.0 then
      newy := 1.0 +. !newy;
    assert (!newx >= 0.0 && !newx <  1.0 && !newy >= 0.0 && !newy <  1.0);
    (!newx, !newy);
)


let center_and_reflect_nodes nodes point = (

  let delta = (0.5, 0.5) ---. point in
  let offset_nodes = Array.map (fun p -> delta +++. p) nodes in
    Array.map reflect offset_nodes ;
)


(* 
   takes two points as [|(x1, y1); (x2, y2)|]. (normalized, ie all components in [0,1])
   If the shortest path between points is direct, returns as is. 
   If the shortest path is via wrapping over the boundary, returns two segments showing the wrapping
*)

let reflect_segment seg = (

  if (Array.length seg) != 2 then 
    raise (Failure (Printf.sprintf "reflect_segment: got segment of length %d" (Array.length seg)));
  let c_and_r node refnode = 
    Array.get (center_and_reflect_nodes [|node|] refnode) 0 in
  let p1 = Array.get seg 0 and p2 = Array.get seg 1 in
  let d1 = dist_sq p1 p2 and d2 = dist_sq (0.5,0.5)  (c_and_r p1 p2) in

  if ( abs_float (d1 -. d2) < 0.01 ) then
    seg
  else
    complement_segment seg
)      
      
let ler_draw_segment a = (
  assert (Array.length a == 2);
  let scaled = scale_points a in
  Graphics.draw_segments [| xx scaled.(0), yy scaled.(0), xx scaled.(1), yy scaled.(1)|]
)

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
      let pt =  (i2f i) /. (i2f n)  in
      ler_draw_segment [| (pt, 0.0); (pt, 1.0)|];
      ler_draw_segment [| (0.0, pt); (1.0, pt)|];
     end
   done
 end

let disc_draw_gradient gradient_matrix = (
  for i = 0 to (Array.length gradient_matrix - 1) do 
    for j = 0 to (Array.length gradient_matrix.(0) - 1) do 
      ler_draw_segment [|coord_i2f (i, j) ; (coord_i2f  (i, j)) +++. ((gradient_matrix.(i).(j)) ///. 2.0)|]
    done;
  done;
)

let cont_draw_gradient gradient_array = (
  Array.iter 
  (fun (pos, grad) -> (
    Printf.printf "Drawing grad %s at pos %s\n" (Coord.sprintf grad) (Coord.sprintf pos);
    ler_draw_segment [|pos; pos +++. grad|];
  )
  ) gradient_array
)

let draw_cross point w = (
  let quad_of_pairs p1 p2 = (xx p1, yy p1, xx p2, yy p2) in
  let wx = (w, 0) and wy = (0, w) in
  let p = scale_pos point in
    Graphics.draw_segments [| (quad_of_pairs (p --- wx) (p +++ wx)); 
			      (quad_of_pairs (p --- wy) (p +++ wy)); 
			   |];
)


let hop_col_color ~hop  = (
  [| Graphics.black; Graphics.red;
  Graphics.blue |].(hop mod 3)
)

let hop_col_bw =
Graphics.rgb 0 0 0


(*
let draw_route ~color ~route = (

  let len =  Route.length route in
  let rec draw_disks_ route = (

    match route with 
      | [] -> ()
      | hop1::hop2::r -> (
	  Graphics.set_color (Graphics.rgb 100 100 100);    
	  circle_nodes ~fill:false [|hop1.Route.hop|] (hop1.Route.searchcost);
	  draw_disks_ (hop2::r);
	)
      | hop1::r -> ()
  ) in
  let rec fill_disks_ route = (

    match route with 
      | [] -> ()
      | hop1::hop2::r -> (
	  Graphics.set_color (Graphics.rgb 230 230 230);    
	  circle_nodes ~fill:true [|hop1.Route.hop|] (hop1.Route.searchcost);
	  fill_disks_ (hop2::r);
	)
      | hop1::r -> ()
  ) in

  let rec draw_route_ route = (
    
    match route with 
      | [] -> ()
      | hop1::hop2::r -> (
(*	  Graphics.set_color (color ~hop:!i ~routelength:len);*)
	  Graphics.set_color (Graphics.rgb 0 0 0);
	  ler_draw_segments_reflect [|hop1.Route.hop; hop2.Route.hop|];
	    let n = hop1.Route.hop in
	    let c1 = xx (scale_pos n)  
	    and c2 = yy (scale_pos n)  in
	    let segments = [|(c1 - 3, c2, c1 + 3, c2) ; (c1, c2 - 3, c1, c2 + 3)|] in
	    Graphics.draw_segments segments ;
	    draw_route_ (hop2::r);
	)
      | hop1::r -> ()
  )
  in 
  let src = Route.nth_hop route 0 in
  let dst = Route.last_hop route in
  let dist = (World.w())#dist_coords src.Route.hop dst.Route.hop in
  Graphics.set_color (Graphics.rgb 245 245 245);    
  circle_nodes ~fill:true [|src.Route.hop|] dist;

  draw_disks_ route;
  fill_disks_ route;
  draw_route_ route;
  Graphics.set_color Graphics.black
)
*)




let mouse_choose_node get_nodes_at msg = (
  printf "%s \n" msg; flush Pervasives.stdout;
  let s1 = Graphics.wait_next_event [Graphics.Button_down] in
  get_nodes_at ~unitpos:(unscale_pos (s1.Graphics.mouse_x, s1.Graphics.mouse_y))
)




let dump_window outfile = (
  let colorarray = Graphics.dump_image (Graphics.get_image 0 0 (Graphics.size_x ()) (Graphics.size_y()))  in
  let fd = open_out_bin outfile in
  Marshal.to_channel fd colorarray [];
  close_out fd
)



