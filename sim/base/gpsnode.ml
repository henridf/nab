



open Printf
open Misc


class gpsnode ~(pos_init:Coord.coordf_t) id   = 

object
  
  inherit Simplenode.simplenode id


  val mutable pos = pos_init

  method pos = 
    (World.w())#nodepos id

end



