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



