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
