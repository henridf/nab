(*
 *
 *  nab - Network in a Box
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

open L2pkt
open Maca_pkt
open Printf
open Misc

let rndseed = ref 0 

let macs_array_ = 
  Array.init Simplenode.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i

class maca_contentionmac ?(stack=0) ~bps (owner:#Simplenode.simplenode) =
object(s)
  inherit Log.inheritable_loggable
  inherit MACA_backend.maca_backend ~stack ~bps owner as backend
  inherit Contention_frontend.contention_frontend ~stack ~bps owner as frontend

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/maca_contentionmac";
    Hashtbl.replace macs_array_.(stack) owner#id (s :> maca_contentionmac);
    incr rndseed
  )

  method reset_stats =
    frontend#frontend_reset_stats; 
    backend#backend_reset_stats

  method other_stats = frontend#frontend_stats, backend#backend_stats

end

