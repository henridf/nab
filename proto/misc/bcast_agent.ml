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

(** A simple agent to emit and receive bcast packets.
  @author Thomas Schmid.
 *)


(** @param theowner a {!Node.node} object representing the node on which
  this agent is running *)
class bcast_agent ?(stack=0) theowner = 
object(s)
  inherit [unit] Rt_agent_base.base ~stack theowner 

  initializer (
    s#set_objdescr 
    ~owner:(theowner :> Log.inheritable_loggable) "/Bcast_Agent/";
  ) 

(* This is called by the containing node each time we receive any packet. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = (

    (* Check what type of packet this is, (we are only interested in bcast
       packets). *)
    match (L3pkt.l4pkt l3pkt) with
	
      | `EMPTY -> (* This is a hello packet *)

	  let src = L3pkt.l3src l3pkt 
	    (* the sender of this hello message *)
	  in

	  s#log_debug (lazy (sprintf "Received empty packet from %d" src));

      | _ -> () (* ignore any other type of packet *)
)

  method private construct_bcast_pkt l4pkt dst = (
    (* Make protocol-specific part of l3 header. For this simple hello
       agent, this is just the position. *)
    
    let l3hdr = (* the L3 header, with broadcast dst addr *)
      L3pkt.make_l3hdr ~src:owner#id ~dst:L3pkt.l3_bcast_addr () in
      
      (* stick L3 hdr and L4 payload together to obtain
	 full L3 packet *)
      L3pkt.make_l3pkt ~l3hdr ~l4pkt 
  )

  method private recv_pkt_app l4pkt dst = (

    s#log_info (lazy (sprintf "Received packet from upper-layer packet for dst %d" dst));

    let l3pkt = s#construct_bcast_pkt l4pkt dst in
      
      s#mac_bcast_pkt l3pkt;

  )

  method stats = ()
  method reset_stats() = ()
end




