(*                                  *)
(* mws  multihop wireless simulator *)
(*                                  *)

open Printf

(** A bunch of simple agents who emit and receive hello packets and 
  maintain a last-encounter table *)

(** @param owner a {!Gpsnode.gpsnode} object representing the node on which
  this agent is running *)
class base_hello_agent owner = 
object(s)
  inherit Log.inheritable_loggable

  val owner:Gpsnode.gpsnode = owner

  (** This {!NodeDB.nodeDB} object is our last-encounter table *)
  val mutable db = new NodeDB.nodeDB (Param.get Params.ntargets)

  method db = db
  method set_db thedb = db <- thedb

  initializer (
    objdescr <- (owner#objdescr ^  "/Hello_Agent ");
(*    (Gworld.world())#add_new_ngbr_hook owner#id ~hook:s#add_neighbor*)
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
	  (* This encounter value encapsulates time & place of this encounter *)
	  let encounter = 
	    (Common.enc ~time:(Common.get_time()) ~place:pos) 
	  in

	  (* Add in our last-encounter table that node src was at place pos at current
	     time *)
	  db#add_encounter 
	    ~nid:src 
	    ~enc:encounter
	    
      | _ -> () (* ignore any other type of packet *)
)
end




(** A simple hello agent which broadcasts a hello packet every second *)
class periodic_hello_agent owner = 
object (s)
  inherit base_hello_agent owner as super
    
  initializer (
    (* Schedule the first hello broadcast for in one second *)
    (Gsched.sched())#sched_in ~f:(s#send_hello) ~t:1.0
  )


  method send_hello() = (

    (* Prepare a hello packet for broadcasting *)

    let l3hdr = (* the L3 header, with broadcast dst addr *)
      L3pkt.make_l3hdr 
	~srcid:owner#id
	~dstid:L3pkt._L3_BCAST_ADDR
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
    (Gsched.sched())#sched_in ~f:(s#send_hello) ~t:1.0
  )
end   




