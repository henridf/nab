(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Misc

(* Tracing facilities. This is for spitting out naml-readable messages 
   to be interpreted by naml *)

let set_trace_chan, oc = (
  let chan = ref None in 
  let set_ ch = chan := Some ch in
  let outchan_ () = match !chan with
    | None -> raise (Failure "Channel not set via Trace.set_trace_chan")
    | Some c -> c
  in
  (set_, outchan_)
)

let close_trace_chan () = close_out (oc())
let namltrace ~msg = 
  if (Param.get Params.trace_enabled) then
    let stamped_msg = Naml_msg.mk_stamped_msg ~t:(Common.get_time()) ~msg:msg in
    Marshal.to_channel (oc()) stamped_msg []
