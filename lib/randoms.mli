(*
 *
 *  Fake - a network simulator
 *  Henri Dubois-Ferriere, LCA/LCAV, EPFL
 * 
 *  Copyright (C) 2004 Laboratory of Audiovisual Communications (LCAV), and
 *  Laboratory for Computer Communications and Applications (LCA), 
 *  Ecole Polytechnique Federale de Lausanne (EPFL),
 *  CH-1015 Lausanne, Switzerland
 *
 *  This file is part of fake. Fake is free software; you can redistribute it 
 *  and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version. 
 *
 *  Fake is distributed in the hope that it will be useful, but WITHOUT ANY
 *  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *  details (enclosed in the file GPL). 
 *
 *)

(* $Id$ *)







(** Wrappers around the standard library [Random] module, to allow rewinding
  of the global RNG stream.

  This is useful for example if we have a simulation
  script where we wish to re-run the same scenario (with identical mobility,
  traffic pattern) many times, with a different protocol set up at each time. 
  With this we can set the seed back to its starting value and be sure that we
  are testing each protocol over exactly the same inputs.

  @author Henri Dubois-Ferriere.
*)


val change_seed : ?newseed:int -> unit -> unit
  (** Change the RNG seed. *)

val rewind_seed : unit -> unit
  (** Bring the RNG seed back to the value it was given in the last call to
    {!Randoms.rewind_seed}, or it's initial value if {!Randoms.rewind_seed} has never been called. *)



