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

(* $Header *)



module Pms = Params
module Pm = Param

module Config = 
struct
  let nruns = 
    Pm.intcreate
      ~name:"nruns" 
       ~default:1
      ~cmdline:true
      ~doc:"The number of runs" ()

  let nsinks = 
    Pm.intcreate
      ~name:"nsinks"
      ~default:1
      ~cmdline:true
      ~doc:"Number of sinks" ()
      ~checker:(fun n -> Diff_agent.nsinks_ := Some n)

  let floodint = 
    Pm.floatcreate
      ~name:"floodint"
      ~default:60.
      ~cmdline:true
      ~doc:"Mean interval between floods" ()
      ~checker:(fun n -> Diff_agent.mean_interest_interval := n)

  let duration = 
    Pm.floatcreate
      ~name:"duration"
      ~default:60.
      ~cmdline:true
      ~doc:"Simulation duration" ()
      
  let difftype = 
    Pm.stringcreate ~name:"difftype" ~default:"Voronoi" ~cmdline:true
      ~doc:"Diffusion algorithm" 
      ~checker:(fun s -> Diff_agent.strset_difftype s) ()

end
