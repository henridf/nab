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


class nullmac_q ?(stack=0) ~bps owner  = 
  let myid = owner#id in
object(s)
  inherit Log.inheritable_loggable
  inherit Mac_base.null_frontend ~stack ~bps owner as frontend
  inherit Mac_base.queue_backend ~stack ~bps owner as backend

  val rnd = Random.State.make [|!rndseed|]

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/nullmac_q";
    Hashtbl.replace macs_array_.(stack) owner#id (s :> nullmac_q);
    incr rndseed
  )

  method bps = bps

  method reset_stats = 
    frontend#frontend_reset_stats;
    backend#backend_reset_stats

  method other_stats = backend#backend_stats

end

