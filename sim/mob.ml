open Coord
open Graph
open Misc



(* xxx/hack copied from gui_hooks b/c otherwise makefile problems in using
   Gui_hooks.* from here *)
let x_pix_size() = Param.get Params.x_pix_size
let y_pix_size() = Param.get Params.y_pix_size

let x_mtr() = Param.get Params.x_size
and y_mtr() = Param.get Params.y_size


let x_mtr_to_pix x = f2i ((i2f (x_pix_size())) *. (x /. x_mtr()))
let y_mtr_to_pix y = f2i ((i2f (y_pix_size())) *. (y /. y_mtr()))

let x_pix_to_mtr x = (x_mtr()) *. ((i2f x) /. (i2f (x_pix_size())))
let y_pix_to_mtr y = (y_mtr()) *. ((i2f y) /. (i2f (y_pix_size())))

let pos_mtr_to_pix pos = 
  (x_mtr_to_pix (Coord.xx pos), y_mtr_to_pix (Coord.yy pos))

let pos_pix_to_mtr pos = 
  (x_pix_to_mtr (Coord.xx pos), y_pix_to_mtr (Coord.yy pos))




class virtual mobility 
  (owner:#Simplenode.simplenode) 
  ?gran
  () =
object(s)
  inherit Log.inheritable_loggable 

  val owner = owner
  val mutable speed_mps =  1.0
  val mutable moving =  false
  val mutable granularity = 1.0 (* be careful if high speed and low granularity, then
			   we will load the scheduler with zillions of small
			   movement events *)

  val seqno = ref 1
    (* Monontonically increasing seqno used to disqualify a #move event that remains in the scheduler
       after #stop is called.  
       (We have to resort to this trick cos no way to cancel events in
       scheduler)
    *)


  initializer (
    match gran with 
      | Some g -> granularity <- g
      | _ -> ()
  ) 

  method start = (
    s#log_info (lazy "Starting.");
    if (not moving) then (
      moving <- true;
      s#move !seqno
    )
  )

  method stop = (
    if (moving) then ( 
      incr seqno;
      s#log_info (lazy "Stopping.");
      moving <- false
    )
  )

  method set_speed_mps speed = speed_mps <- speed

    
  (* should move us by gran meters .
     not sure if this gran parameter makes sense in other mobs (like rw)
  *)
  method virtual getnewpos : gran:float -> Coord.coordf_t
    
  method private move sn  = (

    if (sn > !seqno) then (failwith "Mob.move: got sn bigger than expected");
    if (sn = !seqno) then (
      (* After #stop() is called, we have one outstanding movement event
	 still to fire, which we ignore here by the above check. *)
	
      let newpos = s#getnewpos granularity in 
      (Gworld.world())#movenode ~nid:owner#id ~newpos:newpos;
      s#log_debug (lazy (Printf.sprintf "moving to %s" (Coord.sprintf newpos)));
      (*   (Gworld.world())#movenode ~nid:owner#id ~newpos:newpos;*)

      let sncpy = !seqno in (* to avoid ref problem *)
      let move_event() = s#move sncpy in
      (* mob is assumed to move us by granularity [meters], so we should schedule the next
	 one in granularity / speed_mps seconds.   *)
      (Gsched.sched())#sched_in ~f:move_event ~t:(granularity /. speed_mps)
    )
  )

end


class virtual waypoint
  (owner:#Simplenode.simplenode) 
  ?gran
  () = 
object(s)
  inherit mobility owner ?gran  ()
  val mutable target_ = (0.0, 0.0)

  initializer (
    s#set_objdescr ~owner:(owner :> #Log.inheritable_loggable)  "/waypoint";
    target_ <- (Gworld.world())#random_pos
  )


  method virtual private new_waypoint : unit -> Coord.coordf_t

   method getnewpos ~gran = (
    
     let pos = (Gworld.world())#nodepos owner#id in
     assert (((Gworld.world())#boundarize pos) = pos);
     if ((Gworld.world())#dist_coords target_ pos) <= gran then (
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
  method private new_waypoint() = (Gworld.world())#random_pos
end

class borderwaypoint 
  (owner:#Simplenode.simplenode)
  ?gran
  () =
object
  inherit waypoint owner ?gran  ()
    
  method private new_waypoint() = 
    let x = Random.float (Param.get Params.x_size)
    and y = Random.float (Param.get Params.y_size) 
    in
    match (Random.bool(), Random.bool()) with
      | true, true -> x, (Param.get Params.y_size)
      | true, false -> x, 0.0
      | false, true -> 0.0, y
      | false, false -> (Param.get Params.y_size), y
	  
end



let closest_epfl_node pos = (
  let d, ind = (ref max_float, ref (-1)) in 
  Graph.iteri_
    (fun i -> 
      let thisdist = 
	(Gworld.world())#dist_coords 
	(pos_pix_to_mtr (Read_coords.box_centeri i))
	pos 
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
  inherit mobility owner ()

  val mutable graphtarget_ = 0      (* as a graph node index *)
  val mutable graph_hops_ = []       (* remaining hops through the graph to
					graphtarget_ *)
  val mutable current_graph_pos_ = 0

  initializer (
    s#set_objdescr ~owner:(owner :> #Log.inheritable_loggable) "/epfl_waypoint";
    current_graph_pos_ <- closest_epfl_node ((Gworld.world())#nodepos owner#id);
    s#get_new_target;
  )
    
  method private get_new_target = (

    let g = graphtarget_ in
    (* to make sure we pick a different one *)
    while (g = graphtarget_) do
      graphtarget_ <-  Random.int 113;
    done;

    current_graph_pos_ <- closest_epfl_node ((Gworld.world())#nodepos owner#id);
    graph_hops_ <- 
    List.map (fun i -> 
      pos_pix_to_mtr ( Read_coords.box_centeri i)
    ) 
      ((Graph.routei_dij_ (Read_coords.g()) current_graph_pos_ graphtarget_) @
      [graphtarget_]);

  )
    
  method getnewpos ~gran = (
    let next_hop_target = List.hd graph_hops_ in
    let pos = ((Gworld.world())#nodepos owner#id) in
    if ((Gworld.world())#dist_coords next_hop_target pos) <= gran then (
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

  inherit mobility owner ()

  (* ignores gran, meaningless for a discrete mob *)
  method getnewpos ~gran = 
    let pos = ((Gworld.world())#nodepos owner#id) in
    assert ((isint (xx pos) ) && (isint (yy pos)));
    let step = 
      [|
	(1., 0.); 
	(-1., 0.); 
	(0., 1.); 
	(0., -1.)
      |].(Random.int 4) in
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
  inherit mobility "rj"

  method initialize () = ()
  method getnewpos ~node = 
    (Gworld.world())#random_pos
  method move ~node  = 
    node#move (Gworld.world())#random_pos
end


*)

