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










let nodes = 
  Param.intcreate 
    ~name:"nodes" 
    ~default:500
    ~cmdline:true
    ~doc:"Number of nodes"
    ()

let x_size = 
  Param.floatcreate 
    ~name:"x_size" 
    ~doc:"X [m] size  of simulation area"
    ()

let y_size = 
  Param.floatcreate 
    ~name:"y_size" 
    ~doc:"Y [m] size of simulation area"
    ()

let x_pix_size = 
  Param.intcreate 
    ~default:1200
    ~name:"x_pix_size" 
    ~doc:"X [pix] size  of simulation area"
    ()

let y_pix_size = 
  Param.intcreate 
    ~name:"y_pix_size" 
    ~default:900
    ~doc:"Y [pix] size of simulation area"
    ()

let rrange = 
  Param.floatcreate 
    ~name:"rrange" 
    ~doc:"Radio range [m] of nodes"
    ()

let ntargets = 
  Param.intcreate 
    ~name:"ntargets" 
    ~default:1
    ~doc:"Number of targets in Simulation"
    ()

let mac = 
  Param.stringcreate ~name:"mac" ~default:"null" ~cmdline:true
    ~doc:"Mac layer" ~checker:(fun s -> Mac.strset_mac s) ()

(*
let world = 
  Param.stringcreate ~name:"world" ~default:"null" ~cmdline:true
    ~doc:"Mac layer" ~checker:(fun s -> Mac.strset_mac s) ()
*)
let log_level = 
  Param.stringcreate ~name:"loglevel" ~default:"notice" ~cmdline:true
    ~doc:"Log level"  ~checker:(fun s -> Log.strset_log_level s) ()

