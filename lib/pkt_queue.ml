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



type stats =
    {mutable dropped : int; mutable inpkts : int; mutable outpkts : int}

type 'a t = 'a Queue.t * int * stats
    
let create size = 
  if size < 0 then failwith "Pkt_queue.create"; 
  (Queue.create(), size, {dropped = 0; inpkts = 0; outpkts = 0})

let stats (_, _, stats) = 
  (* make copy to avoid bugs with mutables *)
  {stats with dropped = stats.dropped}


let push el (q, max, stats) = 
  if Queue.length q = max then (
    stats.dropped <- stats.dropped + 1;
    false
  ) else (
    Queue.add el q;
    stats.inpkts <- stats.inpkts + 1;
    true
  )

let pop (q, _, stats) = 
  stats.outpkts <- stats.outpkts + 1;
  Queue.pop q


let peek (q, _, _) = Queue.peek q

let reset_stats_ qstats = 
    qstats.dropped <- 0;
    qstats.inpkts <- 0;
    qstats.outpkts <- 0

let clear ?(stats=true) (q, _, qstats) = 
  if stats then reset_stats_ qstats;
  Queue.clear q

let reset_stats (_, _, qstats) = reset_stats_ qstats

let copy (q, _, _) = Queue.copy q
let is_empty (q, _, _) = Queue.is_empty q
let length (q, _, _) = Queue.length q
let iter f (q, _, _) = Queue.iter f q
let fold f init (q, _, _) = Queue.fold f init q

let transfer _ _  = raise Misc.Not_Implemented
  (* because not clear if this is needed for pktqueues, and if so what to 
     do with stats *)



