(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf
open Misc


class gpsnode ~(pos_init:Coord.coordf_t) id   = 

object(s)
  
  inherit Simplenode.simplenode id


  val mutable pos = pos_init

  method pos = 
    (Gworld.world())#nodepos id

end



