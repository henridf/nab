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







(** Discrete event schedulers.
  @author Henri Dubois-Ferriere.
*)



(** 
  There are currently two schedulers:

  - {!Sched.schedList} is list-based, with O(n) insert time and O(1) pop
  time. Multiple events scheduled to be run at the same time
  (or at ASAP) are executed in FIFO order.

  - {!Sched.schedHeap} is heap-based, with O(logn) insert and O(logn) pop time.
  Multiple events scheduled at the same time (or at ASAP) are executed in
  undefined order.

*)




(**  [schedList] : a list-based scheduler, with O(n) insert time and O(1) pop
  time. Multiple events scheduled to be run at the same time
  (or at ASAP) are executed in FIFO order.
*)
class schedList : Scheduler.t

(**  
  [schedHeap] : A heap-based, with O(logn) insert and O(logn) pop time.
  Multiple events scheduled at the same time (or at ASAP) are executed in
  undefined order.
*)
class schedHeap : Scheduler.t

val set_sched : Scheduler.t -> unit
  (** Sets the global scheduler object. *)

val s : unit -> Scheduler.t
  (** Returns the global scheduler object, if one has been set. *)

