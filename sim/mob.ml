open Coord
open Graph
open Misc



(* xxx/hack copied from gui_hooks b/c otherwise makefile problems in using
   Gui_hooks.* from here *)
let x_pix_size = ref 1200
let y_pix_size = ref 900

let x_mtr() = Param.get Params.x_size
and y_mtr() = Param.get Params.y_size

let x_mtr_to_pix x = f2i ((i2f !x_pix_size) *. (x /. x_mtr()))
let y_mtr_to_pix y = f2i ((i2f !y_pix_size) *. (y /. y_mtr()))

let x_pix_to_mtr x = (x_mtr()) *. ((i2f x) /. (i2f !x_pix_size))
let y_pix_to_mtr y = (y_mtr()) *. ((i2f y) /. (i2f !y_pix_size))

let pos_mtr_to_pix pos = 
  (x_mtr_to_pix (Coord.xx pos), y_mtr_to_pix (Coord.yy pos))

let pos_pix_to_mtr pos = 
  (x_pix_to_mtr (Coord.xx pos), y_pix_to_mtr (Coord.yy pos))




class virtual mobility 
  (abbrevname:string) 
  (owner:#Simplenode.simplenode) 
  (movefun:(newpos:Coord.coordf_t -> unit)) =
object(s)
  val abbrev = abbrevname
  val owner = owner
  val mutable speed_mps =  1.0
  val mutable moving =  false
  val granularity = 1.0 (* be careful if high speed and low granularity, then
			   we will load the scheduler with zillions of small
			   movement events *)

  val movefun = movefun
  method start = (
    if (not moving) then (
      moving <- true;
      s#move
    ) 
  )

  method stop = (
    if (moving) then 
      moving <- false
  )

  method set_speed_mps speed = speed_mps <- speed

    
  (* should move us by gran meters .
     not sure if this gran parameter makes sense in other mobs (like rw)
  *)
  method virtual getnewpos : gran:float -> Coord.coordf_t
    
  method private move  = (

    (* if we are stopped then we ignore previously scheduled mobility events. 
       this is for example so that in the gui case, when we call sched#run() to compute
       a route, we can be sure that no node will move during the route computation *)
    if (moving) then (
      let newpos = s#getnewpos granularity in 
      movefun ~newpos:newpos;
(*      (Gworld.world())#movenode ~nid:owner#id ~newpos:newpos;*)

      (* mob is assumed to move us by granularity [meters], so we should schedule the next
	 one in granularity / speed_mps seconds.
      *)
      let move_event() = s#move in
      (Gsched.sched())#sched_in ~f:move_event ~t:(granularity /. speed_mps)
    )
  )

  method abbrevname = abbrev (* for making filenames etc *)
end


class waypoint 
  (owner:#Simplenode.simplenode) 
  (movefun:(newpos:Coord.coordf_t -> unit)) = 
object 
  inherit mobility "wp" owner movefun
  val mutable target_ = (0.0, 0.0)

  initializer (
    target_ <- (Gworld.world())#random_pos
  )

   method getnewpos ~gran = (
    
     let pos = (Gworld.world())#nodepos owner#id in
     assert (((Gworld.world())#boundarize pos) = pos);
    if ((Gworld.world())#dist_coords target_ pos) <= gran then (
      (* arrived within gran[m] of target *)
      let oldtarget = target_ 
      in
      target_ <- (Gworld.world())#random_pos;
      oldtarget
    ) else (
      let direction =  (Coord.normalize (target_ ---. pos))   in
      (pos +++. (direction ***. gran))
    )
  )


end


let get_containing_node pos = (
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
  (movefun:(newpos:Coord.coordf_t -> unit)) = 
object(s)
  inherit mobility "epfl" owner movefun

  val mutable graphtarget_ = 0      (* as a graph node index *)
  val mutable graph_hops_ = []       (* remaining hops through the graph to
				       graphtarget_ *)
  val mutable current_graph_pos_ = 0


  initializer (
    current_graph_pos_ <- get_containing_node ((Gworld.world())#nodepos owner#id);
    s#get_new_target;
  )
    
  method private get_new_target = (

    let g = graphtarget_ in
    (* to make sure we pick a different one *)
    while (g = graphtarget_) do
      graphtarget_ <-  Random.int 113;
    done;

    current_graph_pos_ <- get_containing_node ((Gworld.world())#nodepos owner#id);
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
  (movefun:(newpos:Coord.coordf_t -> unit)) = 
object 

  inherit mobility "discrRW" owner movefun

  (* ignores gran, meaningless for a discrete mob *)
  method getnewpos ~gran = 
    let pos = ((Gworld.world())#nodepos owner#id) in
    assert ((isint (xx pos) ) && (isint (yy pos)));
    let step = 
      [|
	(1., 0.); 
	(-1., 0.); 
	(0., 1.); 
	(-1., 0.)
      |].(Random.int 4) in
    let newx = ref (xx (pos +++. step)) 
    and newy = ref (yy (pos +++. step)) in
    
    if (!newx = -1.) then newx := 0.;
    if (!newy = -1.) then newy := 0.;
    if (!newx = (Param.get Params.x_size)) then newx := ((Param.get
      Params.x_size) -. 1.);
    if (!newy = (Param.get Params.y_size)) then newx := ((Param.get
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

