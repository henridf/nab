(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc


class gpsnode ?pos_init id   = 

object(s)
  
  inherit Simplenode.simplenode id


  val mutable pos = 
    if pos_init <> None then (Misc.o2v pos_init) else
    (Gworld.world())#random_pos

  method pos = pos


  method move ~newpos = (
    let oldpos = pos in
    pos <- newpos;
    
    (Gworld.world())#movenode ~nid:id ~newpos;

  )

end









(*
method next_position ~node ~mob = (
    match mob with
      | RANDOMWALK -> 
	  s#reflect_ (
	    node#pos +++. ([|Random.float 2.0; Random.float 2.0|] ---. [|1.0; 1.0|])
	  )
      | WAYPOINT -> raise Misc.Not_Implemented
  )
  *)
