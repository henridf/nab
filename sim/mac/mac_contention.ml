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





open Ether
open L2pkt
open Printf
open Misc

let rndseed = ref 0 


let macs_array_ = 
  Array.init Node.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i




class contentionmac ?(stack=0) ~bps owner  = 
  let myid = owner#id in
object(s)
  inherit Log.inheritable_loggable
  inherit Contention_frontend.contention_frontend ~stack ~bps owner as frontend
  inherit Mac_base.null_backend ~stack ~bps owner as backend


  val rnd = Random.State.make [|!rndseed|] 

  val mutable jitter = 0.1

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/cmac";
    Hashtbl.replace macs_array_.(stack) owner#id (s :> contentionmac);
    incr rndseed
  )

  method private backend_recv = backend#send_up
  method bps = bps

  method reset_stats = 
    frontend#frontend_reset_stats

  method set_jitter j = jitter <- j

  method private xmitdelay l2pkt = 
    let bytes = (L2pkt.l2pkt_size ~l2pkt) in
    (i2f (bytes * 8)) /. bps

    
  method xmit l2pkt = 
    let delay = Random.State.float rnd jitter in
    (Sched.s())#sched_in ~f:(fun () -> frontend#frontend_xmit l2pkt) ~t:delay;
    s#log_debug (lazy (sprintf "Delayed xmit by %f" delay))

  method other_stats = frontend#frontend_stats

end

