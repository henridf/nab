(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


*)



open Coord
open Graph
open Misc

module Random = Random.State 
let rndseed = ref 0


class virtual waypoint
  (owner:#Simplenode.simplenode) 
  ?gran
  () = 
object(s)
  inherit Mob_base.mobility owner ?gran  ()
  val mutable target_ = (0.0, 0.0)

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/waypoint";
    target_ <- (World.w())#random_pos
  )


  method virtual private new_waypoint : unit -> Coord.coordf_t

   method getnewpos ~gran = (
    
     let pos = (World.w())#nodepos owner#id in
     assert (((World.w())#boundarize pos) = pos);
     if ((World.w())#dist_coords target_ pos) <= gran then (
       (* arrived within gran[m] of target *)
       let oldtarget = target_ 
       in
       target_ <- s#new_waypoint();
       oldtarget
     ) else (
       let direction =  (Coord.normalize (target_ ---. pos))   in
       (pos +++. (direction ***. gran))
     )
   )
end


class uniwaypoint 
  (owner:#Simplenode.simplenode) 
  ?gran
  () = 
object
  inherit waypoint owner ?gran  ()
  method private new_waypoint() = (World.w())#random_pos
end

class borderwaypoint 
  (owner:#Simplenode.simplenode)
  ?gran
  () =
object
  inherit waypoint owner ?gran  ()
    
  method private new_waypoint() = 
    let x = Random.float rnd (Param.get Params.x_size)
    and y = Random.float rnd (Param.get Params.y_size) 
    in
    match (Random.bool rnd , Random.bool rnd) with
      | true, true -> x, (Param.get Params.y_size)
      | true, false -> x, 0.0
      | false, true -> 0.0, y
      | false, false -> (Param.get Params.y_size), y
	  
end


open Complex

class billiard
  (owner:#Simplenode.simplenode) 
  ?gran
  () = 
object(s)
  inherit Mob_base.mobility owner ?gran  ()

  val mutable dir_ = {re=0.0; im=0.0}
    (* normalized complex representing direction. *)

  val mutable time_next_dir_change = Time.get_time()

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/billiard";
    dir_ <- s#new_direction
  )

  method private new_direction = 
      let rad = (Random.float rnd (2. *. pi)) -. pi in 
      polar 1.0 rad;

  method private change_dir_maybe = 
    if Time.get_time() > time_next_dir_change then  (

      let rad = (Random.float rnd (2. *. pi)) -. pi in 
      dir_ <- s#new_direction;

      let lambda = 1. /. (speed_mps *. (Param.get Params.x_size)) in 
      (* The inter-change time is an expo; mean is the time to traverse
	 distance of network size. *)
      let delta_next_change = Misc.expo ~rand:(Random.float rnd 1.0) ~lambda in
      time_next_dir_change <- (Time.get_time()) +. delta_next_change
    )



  method getnewpos ~gran = (
    s#change_dir_maybe;

    
    let oldpos = (World.w())#nodepos owner#id in
    if (((World.w())#boundarize oldpos) <> oldpos) then (
      Printf.printf "%s\n %s\n" (Coord.sprintf ((World.w())#boundarize
	oldpos)) (Coord.sprintf oldpos);
      flush stdout;
    );

    assert (((World.w())#boundarize oldpos) = oldpos);
    
    let newx, newy = (oldpos +++. ((dir_.re, dir_.im)) ***. gran) in
    
    let hit_east_border = newx <= 0.0
    and hit_west_border = newx >= (Param.get Params.x_size)
    and hit_south_border = newy <= 0.0
    and hit_north_border = newy >= (Param.get Params.y_size) 
    in

    let newpos = 
    if (hit_east_border || hit_west_border) &&
      (hit_south_border || hit_north_border)
    then 
      begin              
	dir_ <- {re=(minus dir_.re); im=(minus dir_.im)};
	((newx, newy) +++. ((dir_.re, dir_.im)) ***. gran)
      end
    else if (hit_east_border || hit_west_border) then 
      begin              
	dir_ <- {dir_ with re=(minus dir_.re)};
	((newx, newy) +++. ((dir_.re, dir_.im)) ***. gran)
      end
    else if (hit_north_border || hit_south_border) then  
      begin
	dir_ <- {dir_ with im=(minus dir_.im)};
	((newx, newy) +++. ((dir_.re, dir_.im)) ***. gran)
      end
    else 
      newx, newy
    in
    let p = 
      ((World.w())#boundarize newpos) in
p
  )
end



let closest_epfl_node pos = (
  let d, ind = (ref max_float, ref (-1)) in 
  Graph.iteri_
    (fun i -> 
      let thisdist = 
	(World.w())#dist_coords (Read_coords.box_centeri i) pos 
      in
      if thisdist < !d then (
	d := thisdist;
	ind := i
      )
    ) (Read_coords.g());
  !ind
)

  
  
class epfl_waypoint 
  (owner:#Simplenode.simplenode)
  () = 
object(s)
  inherit Mob_base.mobility owner ()

  val mutable graphtarget_ = 0      (* as a graph node index *)
  val mutable graph_hops_ = []       (* remaining hops through the graph to
					graphtarget_ *)
  val mutable current_graph_pos_ = 0

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable) "/epfl_waypoint";
    current_graph_pos_ <- closest_epfl_node ((World.w())#nodepos owner#id);
    s#get_new_target;
  )
    
  method private get_new_target = (

    let g = graphtarget_ in
    (* to make sure we pick a different one *)
    while (g = graphtarget_) do
      graphtarget_ <-  Random.int rnd 113;
    done;

    current_graph_pos_ <- closest_epfl_node ((World.w())#nodepos owner#id);
    graph_hops_ <- 
    List.map (fun i -> Read_coords.box_centeri i) 
      ((Graph.routei_dij_ (Read_coords.g()) current_graph_pos_ graphtarget_) @
      [graphtarget_]);

  )
    
  method getnewpos ~gran = (
    let next_hop_target = List.hd graph_hops_ in
    let pos = ((World.w())#nodepos owner#id) in
    if ((World.w())#dist_coords next_hop_target pos) <= gran then (
      begin
	match graph_hops_ with
	  | hd::[] -> s#get_new_target
	  | hd::rest ->  graph_hops_ <- rest
	  | [] -> raise (Misc.Impossible_Case "Mob.epfl.getnewpos")
      end;
      next_hop_target
    ) else (
      let direction =  (Coord.normalize (next_hop_target ---. pos))   in
      (pos +++. (direction ***. gran))
    )
  )
end

class discreteRandomWalk 
  (owner:#Simplenode.simplenode) 
  () = 
object 

  inherit Mob_base.mobility owner ()

  (* ignores gran, meaningless for a discrete mob *)
  method getnewpos ~gran = 
    let pos = ((World.w())#nodepos owner#id) in
    assert ((isint (xx pos) ) && (isint (yy pos)));
    let step = 
      [|
	(1., 0.); 
	(-1., 0.); 
	(0., 1.); 
	(0., -1.)
      |].(Random.int rnd 4) in
    let newx = ref (xx (pos +++. step)) 
    and newy = ref (yy (pos +++. step)) in
    Printf.printf "Mob.ml: newx %f newy %f\n" !newx !newy; flush stdout;      

    if (!newx = -1.) then newx := 0.;
    if (!newy = -1.) then newy := 0.;
    if (!newx = (Param.get Params.x_size)) then newx := ((Param.get
      Params.x_size) -. 1.);
    if (!newy = (Param.get Params.y_size)) then newy := ((Param.get
      Params.y_size) -. 1.);
    (!newx, !newy)

end


(*

class randomJump = 
object 
  inherit Mob_base.mobility "rj"

  method initialize () = ()
  method getnewpos ~node = 
    (World.w())#random_pos
  method move ~node  = 
    node#move (World.w())#random_pos
end


*)

