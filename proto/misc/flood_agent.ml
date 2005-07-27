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

(** A simple flooding agent: any packet received for the first time
  is reforwarded. 

  @author Henri Dubois-Ferriere.
 *)

let orig_ttl = ref 8


(** @param owner a [Node.node] object representing the node on which
  this agent is running *)
class flood_agent ?(stack=0) theowner = 
object(s)

  inherit [unit] Rt_agent_base.base ~stack theowner 

  val pkt_hash = Hashtbl.create 50;

  initializer (
    s#set_objdescr 
    ~owner:(theowner :> Log.inheritable_loggable) "/Flood_Agent/";
  ) 

  val mutable seqno = 1

  method private incr_seqno() = seqno <- seqno + 1;


  (* This is called from the underlying MAC each time we receive a packet. *)
  method recv_pkt_mac ~l2src ~l2dst l3pkt = (

    let src = (L3pkt.l3src l3pkt) in
    s#log_debug (lazy (sprintf "Received flood packet from src %d" src));
    (* If we've haven't already received this packet, we add to our hashtbl
       and reforward. *)
    if not (Hashtbl.mem pkt_hash l3pkt) then (

      Hashtbl.add pkt_hash l3pkt ();
      if L3pkt.l3ttl l3pkt > 0 then 
	let l3pkt = L3pkt.clone_l3pkt (L3pkt.decr_l3ttl l3pkt) in begin
	  s#log_debug (lazy (sprintf "Packet received (orig %d) for first time;  reforwarding." src));
	  s#mac_bcast_pkt l3pkt end
      else
	s#log_debug (lazy (sprintf "TTL reached zero, dropping packet." ));
    ) else (
      s#log_debug (lazy (sprintf "Duplicate packet (orig %d); dropping" src));
    )
  )


  method private construct_flood_pkt l4pkt dst = (
    (* Make protocol-specific part of l3 header. For this simple flooding
       agent, this is just the sequence number. *)

    let hdr_ext = Simple_pkt.make_simple_hdr ~seqno:seqno in

    (* Make complete l3 header, which contains src, dst, and our protocol specific
       extension. *)
    let l3hdr = 
      L3pkt.make_l3hdr ~src:myid ~ttl:(!orig_ttl - 1) ~dst ~ext:(`SIMPLE_HDR hdr_ext) () in
    
    (* Return a full l3 pkt containing the constructed l3 hdr and the l4 packet we
       were passed.*)
    L3pkt.make_l3pkt ~l3hdr ~l4pkt
  )

    (* [app_recv_l4pkt] is the entry point from upper (L4) layers which have a 
       packet to send. We build the L3 header and broadcast the packet. *)
  method private recv_pkt_app l4pkt dst = (

    s#log_info (lazy (sprintf "Received packet from upper-layer packet for dst %d" dst));
    if  (dst <> myid) then (
      let l3pkt = s#construct_flood_pkt l4pkt dst in
      s#mac_bcast_pkt l3pkt;
      s#log_debug (lazy "Incrementing sequence number after originating a flood packet");
      
      s#incr_seqno()
    )
  )

  method stats = ()
  method reset_stats() = ()

end
