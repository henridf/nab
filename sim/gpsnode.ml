(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc


class gpsnode  ~pos_init ~id   = 

object(s)
  
  inherit Simplenode.simplenode ~id


  val mutable pos = pos_init

  val mutable mob_mhooks = []

  method pos = pos

  method add_mob_mhook  ~hook =
    mob_mhooks <- hook::mob_mhooks

  method move ~newpos = (
    let oldpos = pos in
    pos <- newpos;
    
    List.iter 
      (fun mhook -> mhook newpos (s :> gpsnode) )
      mob_mhooks;
    (Gworld.world())#movenode ~nid:id ~newpos

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
