(* right now, only tests that speed and start/stop state are saved ok. 
   also doesn't test epfl mob yet.
*)

(*#use "/home/henridf/.ocamlinit";;*)

let moving_nodes = [2; 3]
let not_moving_nodes = [1; 11; 12; 13]
let speeds = 
  [
    (1, 0.0); 
    (2, 2.0); 
    (3, 3.0); 
    (11, 2.0); 
    (12, 2.0); 
    (13, 3.0)]


let _ = 
Param.set Params.y_size 1000.;
Param.set Params.x_size 1000.;
Script_utils.init_all();
Script_utils.make_nodes();

Walk.make_discrete_rw_1d (Nodes.node 1);
Walk.make_discrete_rw_2d (Nodes.node 11);
Billiard.make_billiard (Nodes.node 2);
Billiard.make_billiard (Nodes.node 12);

Waypoint.make_uniwaypoint (Nodes.node 3);
Waypoint.make_borderwaypoint (Nodes.node 13);

List.iter (fun (nid, speed) ->
  Mob_ctl.set_speed nid speed) speeds;

List.iter (fun n -> Mob_ctl.start_node n) (1::moving_nodes);

let oc = open_out "/tmp/out" in
Persistency.save_sim oc;
close_out oc



let _ = 
let failed = ref false in
let ic = open_in "/tmp/out" in 
Persistency.restore_sim ic;
let positions_should_change = 
  List.map (fun n -> (World.w())#nodepos n) moving_nodes
and positions_should_not_change = 
  List.map (fun n -> (World.w())#nodepos n) not_moving_nodes in
(Sched.s())#run_for ~duration:60.;

let new_positions_should_not_change = 
  List.map (fun n -> (World.w())#nodepos n) not_moving_nodes in
if new_positions_should_not_change <> positions_should_not_change then (
  failed := true;
  prerr_endline "Static nodes have changed positions!!"
);
let new_positions_should_change = 
  List.map (fun n -> (World.w())#nodepos n) moving_nodes in

List.iter2 (fun p1 p2 -> 
  if p1 = p2 then (
    failed := true;
    prerr_endline "Moving node has not changed positions!!"
  )
) positions_should_change new_positions_should_change;

  
List.iter (fun (nid, speed) ->
  if Mob_ctl.get_speed nid <> speed then (
    failed := true;
    prerr_endline "Speed not correct!!"
  )) speeds;

if !failed then prerr_endline "Test Failed"
  

