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
