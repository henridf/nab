(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Packet
open Printf




class bler_agent owner  = 
object(s)

  inherit Log.loggable

  val owner:Node.node_t = owner

  initializer (
    objdescr <- (owner#objdescr ^  "/Bler_Agent")
  )


  method recv pkt = (
    
    s#log_info (sprintf "%d received pkt with src %d, dst %d"
      owner#id pkt.src pkt.dst);

    match  owner#id = pkt.dst with

      | true -> (* Packet arrived. *)
	  pkt.route <- Route.add_hop pkt.route {
	    Route.hop=owner#id;
	    Route.anchor=owner#id;
	    Route.anchor_age=0.0;
	    Route.searchcost=0.0
	  };
	  s#log_debug (sprintf "packet has arrived");
      | false ->  (* Intermediate Hop *)

	  s#log_debug (sprintf "We are intermediate hop");

	  (* when did we see dst ? *)

	  let our_encounter_age = (

 	    match (owner#db)#last_encounter ~nid:pkt.dst with
	      | None when (pkt.src <> owner#id) 
		  -> raise (Failure "Got bler packet for a node I have never seen\n")
	      | None ->  max_float (* we're src and we've never seen dst *) 
	      | Some enc ->  Common.enc_age enc
	  ) in

	  
	  (* who's seen dst more recently than us? *)

	  let next_hop =  
	    (Gworld.world())#find_closest 
	    owner 
	      (fun n -> if (n#db)#encounter_age pkt.dst < our_encounter_age then true else false)
	  in
	  
	  (* stick us onto route *)

	  pkt.route <- Route.add_hop pkt.route {
	    Route.hop=owner#id;
	    Route.anchor=owner#id;
	    Route.anchor_age=our_encounter_age;
	    Route.searchcost=(((Gworld.world())#dist_nodeids next_hop owner#id) ** 2.0)
	  };

	  (* schedule for forwarding *)

	  let pkt_reception() = (Nodes.node(next_hop))#recv_pkt pkt in
	  (Gsched.sched())#sched_at ~handler:pkt_reception ~t:(Sched.ASAP)
	)

end
