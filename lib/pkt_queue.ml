(*

  Copyright (C) 2004 Swiss Federal Institute of Technology Lausanne (EPFL),
  Laboratory of Audiovisual Communications (LCAV) and 
  Laboratory for Computer Communications and Applications (LCA), 
  CH-1015 Lausanne, Switzerland

  Author: Henri Dubois-Ferriere 

  This file is part of mws (multihop wireless simulator).

  Mws is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  
  Mws is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with mws; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*)

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

let pop (q, max, stats) = 
  stats.outpkts <- stats.outpkts + 1;
  Queue.pop q


let peek (q, max, stats) = Queue.peek q
let clear ?(stats=true) (q, max, qstats) = 
  if stats then (
    qstats.dropped <- 0;
    qstats.inpkts <- 0;
    qstats.outpkts <- 0;
  );
  Queue.clear q

let copy (q, max, stats) = Queue.copy q
let is_empty (q, max, stats) = Queue.is_empty q
let length (q, max, stats) = Queue.length q
let iter f (q, max, stats) = Queue.iter f q
let fold f init (q, max, stats) = Queue.fold f init q

let transfer _ _  = raise Misc.Not_Implemented
  (* because not clear if this is needed for pktqueues, and if so what to 
     do with stats *)



