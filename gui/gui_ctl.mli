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







val startmws : 
  mws_tick:float -> 
  rt_tick_ms:int ->
  display_cb:(unit -> unit) ->
  unit 
  (** Start running according to the following loop:

    - run the mws event loop for [mws_tick] simulator seconds, 
    - call [display_cb] (user-provided func to e.g. update display)

    The loop is invoked every [rt_tick_ms] millisecs. If [real_tick] is
    less than the time it takes to run mws, then there will be backlog.
    Idempotent.
  *)
    
val stop :  unit -> unit
  (** Stop running. Idempotent. *)
