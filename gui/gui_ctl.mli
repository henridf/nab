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







val startsim : 
  sim_tick:float -> 
  rt_tick_ms:int ->
  display_cb:(unit -> unit) ->
  unit 
  (** Start running according to the following loop:

    - run the event loop for [sim_tick] simulator seconds, 
    - call [display_cb] (user-provided func to e.g. update display)

    The loop is invoked every [rt_tick_ms] millisecs. If [rt_tick_ms] is
    less than the time it takes to run [sim_tick] simulated seconds, then
    there will be backlog.
    Idempotent.
  *)
    
val stop :  unit -> unit
  (** Stop running. Idempotent. *)
