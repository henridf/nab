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

(** A simple agent to emit and receive hello packets and 
  maintain a last-encounter table. Mostly for illustrational purposes, since
  these agents do not provide any routing or other capability.

  @author Henri Dubois-Ferriere.
 *)

(** @param owner a {!Gpsnode.gpsnode} object representing the node on which
  this agent is running *)
class base_hello_agent owner = 
object(s)
  inherit Log.inheritable_loggable

  val owner:Gpsnode.gpsnode = owner

  (** This {!Le_tab.le_tab} object is our last-encounter table *)
  val mutable db = new Le_tab.le_tab (Param.get Params.ntargets)

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    objdescr <- (owner#objdescr ^  "/Hello_Agent ");
(*    (World.w())#add_new_ngbr_hook owner#id ~hook:s#add_neighbor*)
  )


(* This is called by the containing node each time we receive any packet. *)
  method mac_recv_l3pkt l3pkt = (

    (* Check what type of packet this is, (we are only interested in hello
       packets). *)
    match (L3pkt.l4pkt l3pkt) with
	
      | `HELLO_PKT pos -> (* This is a hello packet *)

	  let src = L3pkt.l3src ~l3pkt 
	    (* the sender of this hello message *)
	  in

	  s#log_debug (lazy (sprintf "Adding encounter with %d" src));

	  (* Add in our last-encounter table that node src was at place pos at current
	     time *)
	  db#add_encounter 
	    ~nid:src 
	    ~pos

	    
      | _ -> () (* ignore any other type of packet *)
)
end




(** A simple hello agent which broadcasts a hello packet every second *)
class periodic_hello_agent owner = 
object (s)
  inherit base_hello_agent owner as super
    
  initializer (
    (* Schedule the first hello broadcast for in one second *)
    (Sched.s())#sched_in ~f:(s#send_hello) ~t:1.0
  )


  method send_hello() = (

    (* Prepare a hello packet for broadcasting *)

    let l3hdr = (* the L3 header, with broadcast dst addr *)
      L3pkt.make_l3hdr 
	~srcid:owner#id
	~dstid:L3pkt.l3_bcast_addr
	()
    in

    let l4pkt = (* the L4 payload, which contains our position *)
      `HELLO_PKT (owner#pos)
    in 

    let l3pkt = (* stick L3 hdr and L4 payload together to obtain
		   full L3 packet *)
      L3pkt.make_l3pkt ~l3hdr ~l4pkt 
    in

    (* Broadcast out this packet. It will be received by all nodes within range.*)
    owner#mac_bcast_pkt l3pkt;
    
    (* Schedule the next hello broadcast in one second *)
    (Sched.s())#sched_in ~f:(s#send_hello) ~t:1.0
  )
end   




