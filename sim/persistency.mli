(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)


val save_state : out_chan:out_channel -> ntargets:int -> unit
  (* dump current state to out_channel and closes out_channel *)

val read_state : in_chan:in_channel -> unit
  (* restore current state from in_channel. 
     Note that world object should be initialized with the appropriate # of
     nodes before calling this (which is somewhat counterproductive, agreed..)

  *)
