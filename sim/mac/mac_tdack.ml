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

let macs_array_ = 
  Array.init Node.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i

type stats = {
  collsRXRX : int;
  collsRXTX : int;
  dropsTXTX : int;
  dropsTXRX : int;
  failedRX : int;
  ackRX:int;
  ackTX:int;
  ackDouble:int;
  ackTXfailed:int;
  dropRX:int;
  dropTX:int
}

class tdackmac ?(stack=0) ~bps ~chip owner  = 
  let myid = owner#id in
object(s)
  inherit Log.inheritable_loggable
  inherit Tdack_frontend.tdack_frontend ~stack ~bps ~chip owner as frontend
  inherit Tdack_backend.tdack_backend ~stack ~bps owner as backend

  initializer (
    s#set_objdescr ~owner:(owner :> Log.inheritable_loggable)  "/tdackmac";
    Hashtbl.replace macs_array_.(stack) owner#id (s :> tdackmac);
  )

  method bps = bps

  method reset_stats = 
    frontend#tdack_reset_stats;
    backend#backend_reset_stats
      
  method private xmitdelay l2pkt = 
    let bytes = (L2pkt.l2pkt_size ~l2pkt) in
    (i2f (bytes * 8)) /. bps

  method other_stats = frontend#tdack_stats

  method backend_stats = backend#backend_stats

  method private get_stats = 

    let tdack_stats = frontend#tdack_stats in
    let cRXRX = tdack_stats.Tdack_frontend.contention_stats.Contention_frontend.collsRXRX in
    let cRXTX = tdack_stats.Tdack_frontend.contention_stats.Contention_frontend.collsRXTX in
    let dTXTX = tdack_stats.Tdack_frontend.contention_stats.Contention_frontend.dropsTXTX in
    let dTXRX = tdack_stats.Tdack_frontend.contention_stats.Contention_frontend.dropsTXRX in
    let backend_stats = backend#backend_stats in
      {
	collsRXRX = cRXRX;
	collsRXTX = cRXTX;
	dropsTXTX = dTXTX;
	dropsTXRX = dTXRX;
	failedRX =  tdack_stats.Tdack_frontend.failedRX;
	ackRX = backend_stats.Tdack_backend.ackRX;
	ackTX = backend_stats.Tdack_backend.ackTX;
	ackDouble = backend_stats.Tdack_backend.ackDouble;
	ackTXfailed = backend_stats.Tdack_backend.ackTXfailed;
	dropRX = backend_stats.Tdack_backend.dropRX;
	dropTX = backend_stats.Tdack_backend.dropTX
      }



end

