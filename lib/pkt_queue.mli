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

(** Simple packet queue functor. 
  This is basically a standard ocaml queue, with two additions:
  - enforce maximum size (drop-tail on overflow)
  - keep track of some simple statistics 

  Currently all functions from ocaml's [queue.ml] are available on packet
  queues, except for [copy] and [transfer].

  @author Henri Dubois-Ferriere.

*)



type stats = 
    {mutable dropped : int;
    mutable inpkts : int;
    mutable outpkts : int;}
    (** Statistics maintained with each queue. *)    
    
    
type 'a t 
  (** The type of packet queues containing elements of type ['a]. *)

val create : int -> 'a t 
  (** [create s] returns a new queue of maximum size s, initially empty. *)

val stats : 'a t -> stats 
  (** [stats pktq] returns the stats structure of queue [pktq]. *)

val clear : ?stats:bool -> 'a t -> unit 
  (** Discard all elements from a queue. 
    If optional [stats] is false, queue statistics are not reset. 
    Default value of [stats] is true.
  *)

(** 
  Functions which are identical to those in the stdlib's [queue.mli].
*)
val push : 'a -> 'a t -> bool 
val pop : 'a t  -> 'a 
val peek : 'a t -> 'a 
val is_empty : 'a t -> bool 
val length : 'a t -> int 
val iter : ('a -> unit) -> 'a t -> unit 
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a 
