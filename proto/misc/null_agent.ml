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

open Printf

(** A null routing agent: 
  - Incoming packets from MAC layer are passed upwards in the stack if
  addressed to this node, otherwise quietly discarded.. 
  - Outgoing packets (received from upper layer) are directly sent down into
  the MAC.

  This is intended for example when testing MAC layers between in-range nodes.

  Using this routing agent combination with a {!Mac_cheat.cheatmac} one can
  obtain in a simple manner a fully connected mesh, regardless of nodes'
  positions w.r.t radio range.

  @author Henri Dubois-Ferriere.
 *)

(** @param owner a [Simplenode.simplenode] object representing the node on which
  this agent is running *)
class flood_agent ?(stack=0) theowner = 
object(s)

  inherit [unit] Rt_agent_base.base ~stack theowner 

  val pkt_hash = Hashtbl.create 50;

  initializer (
    s#set_objdescr 
    ~owner:(theowner :> Log.inheritable_loggable) "/Null_Agent/";
  ) 


  (* This is called from the underlying MAC each time we receive a packet. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = ()



    (* [app_recv_l4pkt] is the entry point from upper (L4) layers which have a 
       packet to send. We build the L3 header and originate the packet into the
       EASE logic. *)
  method private recv_pkt_app l4pkt dst = (

    s#log_info (lazy (sprintf "Received packet from upper-layer packet for dst %d" dst));
    let l3hdr = L3pkt.make_l3hdr ~src:myid ~dst ~ext:(`NONE) () in 
    let l3pkt = (L3pkt.make_l3pkt ~l3hdr ~l4pkt) in

    s#mac_send_pkt l3pkt dst;

  )

  method stats = ()

end
