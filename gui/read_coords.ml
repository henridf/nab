(*
 *
 *  NAB - Network in a Box
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of NAB. NAB is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  NAB is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)



open Graph
open Coord
open Misc

let gr = ref None
let g () = o2v !gr



let box_centeri i = 
  let pts = Graph.getinfoi_ (g()) i in
  match pts with 
    | x1::y1::x2::y2::x3::y3::x4::y4::[] -> 
	(((Coord.i2f (x1, y1)) +++. (Coord.i2f (x3, y3))) ///. 2.)
   | _ -> raise (Misc.Impossible_Case "Read_coords.box_center")

let box_center n = 
  let i = Graph.index_ (g()) n in
  box_centeri i


let make_graph() = (

  let re = Str.regexp "[ \t]+" in

  gr := Some (Graph.make_ "" (List.length Epflcoords.l_mtr) Graph.Directed);


  List.iter (fun line ->
    let arr = Array.of_list line in
    let node = arr.(0) in
    Graph.add_node_ (g()) node;


    (* right now, because graph library is stupid and only takes int lists for
       node info, we convert positions (mtrs) into ints. *)
    let info = Array.map 
      (fun s -> f2i (float_of_string s)) 
      (Array.sub arr 1 8) in
    Graph.setinfo_ (g()) node (Array.to_list info)
  ) Epflcoords.l_mtr;
  

  List.iter (fun line ->

    let arr = Array.of_list line in
    let node = arr.(0) in
    Array.iteri 
      (fun i ngbr -> 
	if i > 8 then (
(*	  Printf.printf "%s\n" ngbr;*)
	  let d = 
	    sqrt (Coord.dist_sq (box_center node) 
	      (box_center ngbr))
	  in
	  
	  Graph.add_edge_ (g()) node ngbr d;
	)
      ) arr
  ) Epflcoords.l_mtr;

)






let check_conn() = 
  Graph.itern_ (fun src -> 
    Graph.itern_ (fun dst -> 
      Printf.printf "doing %s-%s\n" src dst;
      ignore (Graph.route_dij_ (g()) src dst);
    ) (g())
  ) (g())


let check_ngbrs() = 
  Graph.itern_ (fun n -> 
    let ngbrs = Graph.neigbors_ (g()) n in
    List.iter (fun ngbr ->
      if (not (List.mem n (Graph.neigbors_ (g()) ngbr) )) then (
	Printf.printf "Problem with %s and %s\n" n ngbr; 
	flush stdout
      )
    ) ngbrs
  ) (g())


let _ = 
  make_graph();
  check_ngbrs();

