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



open Coord
open Graph
open Misc

module Random = Random.State 
let rndseed = ref 0

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

  val rnd = Random.make [|!rndseed|]

  val seqno = ref 1
    (* Monontonically increasing seqno used to disqualify a #move event that remains in the scheduler
       after #stop is called.  
       (We have to resort to this trick cos no way to cancel events in
       scheduler)
    *)

  initializer (
    
    begin match gran with 
      | Some g -> granularity <- g
      | None -> granularity <- Param.get (Params.mob_gran)
    end;
    incr rndseed
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
	
      let newpos = s#getnewpos ~gran:granularity in 
      (World.w())#movenode ~nid:owner#id ~newpos:newpos;
(*      s#log_debug (lazy (Printf.sprintf "moving to %s" (Coord.sprintf newpos)));*)
      (*   (World.w())#movenode ~nid:owner#id ~newpos:newpos;*)

      let sncpy = !seqno in (* to avoid ref problem *)
      let move_event() = s#move sncpy in

      (Sched.s())#sched_in ~f:move_event ~t:(granularity /. speed_mps)
    )
  )

end



