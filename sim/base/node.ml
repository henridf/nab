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



open Misc

let sp = Printf.sprintf

let max_nstacks = 8

type node_state_t = {pos:Coord.coordf_t}
let state_pos node_state = node_state.pos


exception Mac_Send_Failure

class node id   = 

object(s)
  
  inherit Log.inheritable_loggable 

  val mutable pktin_mhooks = Array.make max_nstacks []
  val mutable pktout_mhooks = Array.make max_nstacks []
  val mutable macs = Array.make max_nstacks (None : Mac.t option)
  val mutable rt_agents = Array.make max_nstacks (None : Rt_agent.t option)
  val mutable trafficsource_set = false
  val id = id

  initializer (
    s#set_objdescr (sp "/node/%d" id);
    s#log_debug (lazy (sp "New node %d" id));
  )

  method id = id

  method mac ?(stack=0) () = 
    match macs.(stack) with
      | None -> failwith ("Node.mac: No mac on stack "^(string_of_int stack))
      | Some mac -> mac

  method install_mac ?(stack=0) themac = 
    if (macs.(stack) <> None) then 
      failwith "Node.install_mac: mac already there!";
    macs.(stack) <- Some themac

  method remove_mac ?(stack=0) () = 
    macs.(stack) <- None

  method private remove_stack ?(stack=0) () = 
    s#remove_mac ~stack ();
    s#remove_rt_agent ~stack ()

  method install_rt_agent ?(stack=0) (theagent : Rt_agent.t)  = 
    if (rt_agents.(stack) <> None) then 
      failwith "Node.install_rt_agent: agent already there!";
    rt_agents.(stack) <- Some theagent

  method agent ?(stack=0) () = 
    match rt_agents.(stack) with
      | None -> failwith ("Node.agent No rtagent on stack "^(string_of_int stack))
      | Some rtagent -> rtagent

  method private remove_rt_agent ?(stack=0) () = 
    rt_agents.(stack) <- None

  method mac_recv_pkt ?(stack=0) l2pkt = (
    
    let l2dst = (L2pkt.l2dst l2pkt) 
    and l2src = (L2pkt.l2src l2pkt) 
    and l3pkt  = (L2pkt.l3pkt l2pkt) in

    s#log_debug (lazy (sp "Pkt received from source %d on stack %d" 
      (L3pkt.l3src l3pkt) stack));

    (* mhook called before shoving packet up the stack, because 
       it should not rely on any ordering *)
    List.iter 
      (fun mhook -> mhook l2pkt s)
      pktin_mhooks.(stack);
    
    (Opt.may (fun agent -> agent#recv_pkt_mac ~l2src ~l2dst l3pkt))
      rt_agents.(stack);
 
  )
    
  method mac_send_failure ?(stack=0) (l2pkt : L2pkt.t) = (
    let l2dst = (L2pkt.l2dst l2pkt) 
    and l3pkt  = (L2pkt.l3pkt l2pkt) in

    s#log_debug (lazy (sp "Pkt xmit to %d failed" l2dst));

    (Opt.may (fun agent -> agent#mac_callback l3pkt l2dst))
      rt_agents.(stack);
  )    


  method add_pktin_mhook ?(stack=0) f =
    pktin_mhooks.(stack) <- f::pktin_mhooks.(stack)
      
  method add_pktout_mhook ?(stack=0) f =
    pktout_mhooks.(stack) <- f::pktout_mhooks.(stack)
      
  method clear_pkt_mhooks ?(stack=0) () = (
    pktout_mhooks.(stack) <- [];
    pktin_mhooks.(stack) <- []
  )

  method mac_send_pkt ?(stack=0) dst l3pkt = (

    assert (L3pkt.l3ttl l3pkt >= 0);

    let l2pkt = L2pkt.make_l2pkt ~src:id ~dst l3pkt in

    List.iter 
      (fun mhook -> mhook l2pkt s)
      pktout_mhooks.(stack);
    
    (s#mac ~stack ())#xmit l2pkt
  )

  method mac_bcast_pkt ?(stack=0) l3pkt = (

    assert (L3pkt.l3ttl l3pkt >= 0);

    let l2pkt = L2pkt.make_l2pkt ~src:id ~dst:L2pkt.l2_bcast_addr  l3pkt in

    List.iter 
    (fun mhook -> mhook l2pkt s )
      pktout_mhooks.(stack);
    (s#mac ~stack ())#xmit l2pkt;
  )

  method set_trafficsource ~gen ~dst = 
    trafficsource_set <- true;
    match gen() with
      | Some time_to_next_pkt ->
	  let next_pkt_event() = s#trafficsource gen dst in
	  (Sched.s())#sched_in ~f:next_pkt_event ~t:time_to_next_pkt
      | None -> ()

  method clear_trafficsources () = trafficsource_set <- false
	  
  method private trafficsource gen dst = 
    if trafficsource_set then (
      s#originate_app_pkt ~l4pkt:`EMPTY ~dst;
    (* when gen() returns None, this trafficsource is done sending *)
    match gen() with
      | Some time_to_next_pkt ->
	  let next_pkt_event() = s#trafficsource gen dst in
	  (Sched.s())#sched_in ~f:next_pkt_event ~t:time_to_next_pkt
      | None -> ()
    ) 
  method originate_app_pkt ~l4pkt ~dst = 
    let have_agent = ref false in
    Array.iter 
      (Opt.may 
	(fun agent -> agent#recv_pkt_app l4pkt dst; 
	  have_agent :=	true))
      rt_agents;
    if !have_agent = false then
      s#log_warning (lazy "Originated app packet but have no rt agents")

  method dump_state = {pos=(World.w())#nodepos id}

  method pos = (World.w())#nodepos id

end


