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

let rec interpret = function
    (`TOGGLE boolref)::r -> boolref := not !boolref; interpret r
  | (`FUNC f)::r -> f() ; interpret r
  | [] -> ()

let make_radio_buttons (ss_tab : GPack.table) radiobuttonlist top = (
  
  let g = ref None in

  let addbutton (label, actionlist) = 
    let btn = GButton.radio_button ~label () in
    begin
      match !g with 
	| None -> 
	    btn#set_active true; (* first button sets group *)
	    g := Some btn#group
	| Some group -> btn#set_group group
    end;
    ss_tab#attach ~left:0 ~top:!top btn#coerce;
    ignore (btn#connect#released ~callback:(fun _ -> interpret actionlist));
    incr top;
  in
  List.iter addbutton radiobuttonlist
)


let make_buttons (ss_tab : GPack.table) buttonlist top = (
 
  let addbutton (label, buttontype, actionlist) =
    let btn = 
      match buttontype with 
	| `TOGGLE -> GButton.toggle_button  ~label ()
	| `CHECK -> GButton.check_button ~label ()
    in
    let () = ss_tab#attach ~left:0 ~top:!top btn#coerce in

    ignore (btn#connect#released ~callback:(fun _ -> interpret actionlist));
    incr top;
  in

  List.iter addbutton buttonlist;

)
