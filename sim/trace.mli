(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

(* Tracing facilities. This is for spitting out naml-readable messages 
   for interpreting by naml *)
val set_trace_chan : out_channel -> unit
val close_trace_chan : unit -> unit
val namltrace : msg:Naml_msg.msg_t -> unit
