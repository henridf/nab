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



let nodes = 800
let rrange = 12.0
let avg_degree = 8

let pkt_count = ref 0

let setup() = (
  Param.set Params.nodes nodes;

  Param.set Params.rrange rrange;

  let size = Script_utils.size ~rrange ~nodes ~avg_degree () in

  Param.set Params.x_size size;
  Param.set Params.y_size size;

  Script_utils.init_lazy_world();

  Param.set Params.mac "nullmac";

  Script_utils.make_nodes ();
  
  Script_utils.make_flood_agents();

)

let hook pkt node = incr pkt_count

let setup_hooks() = (
  Nodes.iter (fun n -> n#clear_pkt_mhooks());
  Nodes.iter (fun n -> n#add_pktout_mhook hook);
)


let do_flood() = (
  let center_x = (Param.get Params.x_size) /. 2.0
  and center_y = (Param.get Params.y_size) /. 2.0  in
  let originator = 
    Opt.get ((World.w())#find_closest ~pos:(center_x, center_y) ())
  in
  (Nodes.node originator)#originate_app_pkt ~dst:L3pkt.l3_bcast_addr;
  (Sched.s())#run()
)

let print_info() = (
  let avg_ngbrs = Script_utils.avg_neighbors_per_node() 
  in 
  Log.log#log_always (lazy (Printf.sprintf "Avg degree is %f" avg_ngbrs));
  Log.log#log_always (lazy (Printf.sprintf "%d packets were transmitted in the flood" !pkt_count));
)


let main = 
  setup();
  setup_hooks();
  do_flood();
  print_info()

