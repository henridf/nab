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

(* Assumption: on a given stack, there can only be routing agents of the same
   type.*)


let sp = Printf.sprintf

type agent_t = [ `LER | `STR | `AODV ]

module Persist = struct

  let save_agent oc agent = 
    Marshal.to_channel oc agent [];
    begin match agent with
      | `STR -> Str_agent.Persist.save oc
      | `AODV -> Aodv_agent.Persist.save oc
      | `LER -> Ler_agent.Persist.save oc
    end
    
  let save oc = 
    let agents_to_save = 
      ([ `LER; `STR; `AODV ] : [> agent_t] list) in
    (* this cast is to 'open' the type, since when it is read back the type may
       have grown. not sure if not doing this would be dangerous, but playing it
       safe anyway...*)
    
    Marshal.to_channel oc (List.length agents_to_save) [];
    List.iter (fun item -> save_agent oc item) agents_to_save


  
  let restore_agent ic = 
    let agenttype = 
      (Marshal.from_channel ic : agent_t) in
    begin match agenttype with
      | `STR -> Str_agent.Persist.restore ic
      | `AODV -> Aodv_agent.Persist.restore ic
      | `LER -> Ler_agent.Persist.restore ic
    end

  let restore ic = 
    (* we don't attempt to delete any existing rt agents (as we would to the
       restore in mob_ctl for mobs) because of the possible bugs when saving
       restoring rt_agents (which might have events in the scheduler like
       emitting a HELLO). *)    

    let n_agents = (Marshal.from_channel ic : int) in
    if n_agents > 0 then 
      Log.log#log_notice (lazy (sp "Restoring routing agents..."));
    for i = 0 to n_agents - 1 do 
      restore_agent ic
    done;
    if n_agents > 0 then 
      Log.log#log_notice (lazy (sp "Done. (restoring routing agents)"))
	
end
