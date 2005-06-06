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


(** Helper functions for running or restoring from file simulation warmups. *)


val sprint_added_stats : unit -> string
val sprint_added_jdbstats : unit -> string

val setup_or_restore : unit -> unit
  (** If a dump file was passed as an anon argument, restores a warmed up
    simulation using this dump file; otherwise sets up a fresh simulation.*)
  
  
val maybe_warmup : fname:string -> unit
  (** Warmup simulation if -warmup [traffic | mob | none] was passed as
    argument, and save state after warmup to file [fname]. *)
