open Coord
open Misc

(* val xsect_circle :
   center:Coord.coordf_t ->
   radius:float -> segment:Coord.coordf_t array -> bool = <fun> 

   Compute if a grid segment intersects a circle. 
   Since we assume that 
   a) radius > 1.0 (and grid segment side is 1.0), and
   b) the segment does not belong to a tile that is in a cardinal direction
   from the center, 
   we only need check if one end of the segment is in circle and one
   outside. (ie the segment cannot cross the circle in two points) *)

let xsect_circle ~center ~radius ~segment = (
  let t1 = segment.(0) ---. center
  and t2 = segment.(1) ---. center in
  let xsq_plus_ysq p = ((xx p) ** 2.0) +. ((yy p) ** 2.0) in

  ((xsq_plus_ysq t1 < radius ** 2.0) && 
  (xsq_plus_ysq t2 > radius ** 2.0))
  || 
  ((xsq_plus_ysq t1 > radius ** 2.0) && 
  (xsq_plus_ysq t2 < radius ** 2.0))
)

(* returns the four grid segments around point, segments of unit length *)
let faces_around_point point = (
  let center = (coord_floor point) +++. (0.5, 0.5) in
  let north = 0.0,0.5
  and south = 0.0,-0.5 
  and west = -0.5,0.0
  and east = 0.5,0.0 in
  [
    [|center +++. north +++. east; center +++. north +++. west|]; (* ne-nw *)
    [|center +++. south +++. east; center +++. south +++. west|]; (* se-sw *)
    [|center +++. north +++. east; center +++. south +++. east|]; (* ne-se *)
    [|center +++. north +++. west; center +++. south +++. west|]  (* nw-sw *)
  ]
)


(* point: center of a grid tile
   outface: unit grid segment around point 
   -> returns the center of the neighboring grid tile we will land in having
   crossed outface *)
let nextpoint outface point = (
  
  let face_center = (outface.(0) +++. outface.(1)) ///. 2.0 in
  assert ((dist_sq face_center point) = 0.5 ** 2.0);
  (* make sure face and point are coherent *)
  point ---. ((point ---. face_center) ***. 2.0)
)

(*
  val xsect_grid_and_circle : Coord.coordf_t -> float -> Coord.coordi_t list 
*)
let xsect_grid_and_circle ~center ~radius = (
  let rounded_center = ((coord_floor center) +++. (0.5, 0.5)) in
  let north = (rounded_center +++. (0.0, radius)) in

  (*  let north = (((coord_floor center) +++. (0.5, 0.5)) +++. (0.0, radius)) in*)

  let (facein, start_current) = (
    (* we exit through E or S to go clockwise *)
    let southface = [|north +++. (0.5, -0.5); north +++. (-0.5, -0.5)|]
    and northface = [|north +++. (0.5, 0.5); north +++. (-0.5, 0.5)|]
    and eastface = [|north +++. (0.5, -0.5); north +++. (0.5, 0.5)|] 
    and westface = [|north +++. (-0.5, -0.5); north +++. (-0.5, 0.5)|] in 
    let gosouth = (xsect_circle ~center:rounded_center ~radius:radius
      ~segment:southface) 
    and gonorth = (xsect_circle ~center:rounded_center ~radius:radius
      ~segment:northface)
    and goeast = (xsect_circle ~center:rounded_center ~radius:radius
      ~segment:eastface)
    and gowest = (xsect_circle ~center:rounded_center ~radius:radius
      ~segment:westface)
    in 
    match gonorth, gosouth, goeast, gowest with

      |	true, true, false, false  -> raise (Failure " North and South!")
      | true, false, true, false -> (eastface, nextpoint eastface north)
      | true, false, false, true -> (northface, nextpoint northface north)
      | false, true, true, false -> (eastface, nextpoint eastface north)
      | false, true, false, true -> (southface, nextpoint southface north)
      | false, false, true, true -> (eastface, nextpoint eastface north)
      | _ -> raise (Impossible_Case 
	  (Printf.sprintf "Crsearch.xsect_grid_and_circle: rounded_center is %s, radius is %f, values are %b %b %b %b" 
	    (Coord.sprintf rounded_center) radius gonorth gosouth goeast gowest)
	)
  ) in
  
  let se_corner p = coord_f2i (((coord_floor p))) in

  let rec adv facein current l = (
    let outface =     
      List.filter 
	(fun face -> 
	  not (Misc.array_same facein face ) &&
	  xsect_circle ~center:rounded_center ~radius:radius ~segment:face
	)
	(faces_around_point current)
    in 

    if (List.length outface <> 1) then (
      Printf.printf "Crsearch.xsect_grid_and_circle : outface has length %d\n"
      (List.length outface);
      assert (false);
    );
    
    let next = nextpoint (List.hd outface) current in
    if next = north then ((se_corner next)::l) else 
      (se_corner next)::(adv (List.hd outface) next l)
  ) in 
  
  adv facein start_current []
)


(*     
used for visualizing in a toplevel
Ler_graphics.init_gfx();
Ler_graphics.clear_gfx(); Ler_graphics.draw_grid 75;

let color_seq = [| Graphics.red; Graphics.green;
Graphics.blue; Graphics.magenta |] ;;
let color_index = ref 0 ;;

let radius = ref 2.0;;
let next_circle() = Array.of_list (xsect_grid_and_circle ~center:(35.1, 35.1) ~radius:!radius);;
let scaleup p = p *** 10;;

let draw_rects a = (
  Graphics.set_color color_seq.(!color_index mod (Array.length color_seq));
  Array.iter (fun p -> Graphics.fill_rect (xx (scaleup p)) (yy (scaleup p))  10 10) a;
  Graphics.set_color Graphics.black;
  Ler_graphics.draw_grid 75;
  incr color_index
)

let draw_current_circle () = (
  Graphics.set_color Graphics.black;
  Graphics.draw_circle 351 351 (f2i (!radius *. 10.0))
)
*)
