(*
 *
 *  nab - Network in a Box
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

open Ether
open L2pkt
open Maca_pkt
open Printf
open Misc

type state = IDLE | CONTEND | WFCTS | WFDAT | QUIET
let string_of_state = function
  | IDLE -> "IDLE"
  | CONTEND -> "CONTEND"
  | WFCTS -> "WFCTS"
  | WFDAT -> "WFDAT"
  | QUIET -> "QUIET"     

let rndseed = ref 0 

type stats = 
    {colls:Contention_frontend.stats;
    cts_RX:int;
    cts_TX:int;
    rts_RX:int;
    rts_TX:int;
    drops:int
    }

let out_queue_size = 5

let macs_array_ = 
  Array.init Node.max_nstacks (fun _ -> Hashtbl.create (Param.get Params.nodes))
let macs ?(stack=0) () = macs_array_.(stack)
let mac ?(stack=0) i = 
  Hashtbl.find macs_array_.(stack) i

(* Invariants : 
   - if in IDLE state, out_queue is empty.*)



class virtual maca_backend ?(stack=0) ~bps (owner:#Node.node)  = 
  let myid = owner#id in 
object(s)
  
  inherit [stats] Mac_base.backend ~stack ~bps owner as super

  val mutable interfering_until = Time.get_time() -. 1.
  val mutable sending_until = Time.get_time() -. 1.
  val mutable receiving_from = 0
  val rnd = Random.State.make [|!rndseed|] 

  val mutable collsRXRX = 0
  val mutable collsRXTX = 0
  val mutable dropsTXTX = 0
  val mutable dropsTXRX = 0
  val mutable cts_RX = 0
  val mutable cts_TX = 0
  val mutable rts_RX = 0
  val mutable rts_TX = 0
  val mutable drops = 0

  val mutable state = IDLE

  val mutable timer_handle : Scheduler.handle option = None
  val mutable next_timeout : Time.t option = None

  val out_queue = (Pkt_queue.create out_queue_size : L2pkt.t Pkt_queue.t)

  method private backend_reset_stats = 
    collsRXRX <- 0;
    collsRXTX <- 0;
    dropsTXTX <- 0;
    dropsTXRX <- 0;
    cts_RX <- 0;
    cts_TX <- 0;
    rts_RX <- 0;
    rts_TX <- 0;
    drops <- 0;
    (* xxx    super#reset_stats*)


  method private cancel_timeout() = 
    match timer_handle with 
      | None -> ()
      | Some h -> 
	  (Sched.s())#cancel h;
	  timer_handle <- None;
	  next_timeout <- None

  (* Entry into the MACA FSM each time we receive a packet from the radio frontend. *)
  method private backend_recv l2pkt = (
    let dst = l2dst l2pkt in
    let macahdr = L2pkt.maca_hdr l2pkt in
    match state, macahdr with 
      | _, RTS rts when (rts.rts_dst <> myid) -> ()
	  (* Defer rule #1 (applies whatever state we are in).
	     - cancel any existing timer
	     - create timer sufficient to hear CTS 
	     - go to QUIET
	     - if already in QUIET, then compute timeout, and only do above if
	     new timeout is > than current timeout.
	  *)
      | _, CTS cts when (cts.cts_dst <> myid) -> ()
	  (* Defer rule #2 (applies whatever state we are in).
	     - cancel any existing timer
	     - create timer sufficient to hear DATA
	     - go to QUIET
	     - if already in QUIET, then compute timeout, and only do above if
	     new timeout is > than current timeout.
	  *)
      | WFDAT, DATA when (dst = myid) -> ()
	  (* Control rule #4. 
	     - assert that that there is a timer outstanding (if this packet was
	     late and the corresponding timer expired, we would be in IDLE).
	     - cancel timer and go to IDLE
	  *)
      | _, DATA when (dst = myid) -> super#send_up l2pkt
	  (* Receiving unicast DATA when not in WFDAT is not really expected, but
	     hey, might as well grab it!!! *)
      | _, DATA when (dst = l2_bcast_addr) -> super#send_up l2pkt
      | _, DATA when (dst <> l2_bcast_addr) -> ()
	  (* not for us, not a broadcast *)
      | IDLE, RTS rts when (rts.rts_dst = myid) -> ()
	  (* Control rule #2
	     - transmit CTS
	     - set long enough timer to hear data back
	     - go to WFDAT
	  *)
      | IDLE, CTS cts when (cts.cts_dst = myid) -> ()
	  (* unexpected - this should only happen if we had previously sent a
	     RTS to that node, and our WFCTS timer expired. Check that at
	     least we have a packet destined to that node on the top of our
	     queue??
	  *)
      | CONTEND, RTS rts when (rts.rts_dst = myid) -> ()
	  (* Control rule #5 
	     - clear timer
	     - send CTS
	     - go to WFDAT
	  *)
      | CONTEND, CTS cts when (cts.cts_dst = myid) -> ()
	  (* Do nothing *)
      | WFCTS, RTS rts when (rts.rts_dst = myid) -> ()
	  (* Do nothing. *)
      | WFCTS, CTS cts when (cts.cts_dst = myid) -> ()
	  (* Control rule #3. 
	     - clear timer
	     - transmit data packet
	     - go to IDLE
	  *)
      | WFDAT, RTS rts when (rts.rts_dst = myid) -> ()
	  (* Do not answer. *)
      | WFDAT, CTS cts when (cts.cts_dst = myid) -> ()
	  (* Should not happen, do nothing *)
      | QUIET, RTS rts when (rts.rts_dst = myid) -> ()
	  (* Do not answer. *)
      | QUIET, CTS cts when (cts.cts_dst = myid) -> ()
	  (* Do nothing *)
      | _ -> failwith (Printf.sprintf "State : %s. Packet: %s"
	  (string_of_state state) (string_of_maca_pkt macahdr))
  )

  method private go_to_idle() = (
    state <- IDLE;
    (* if out_queue not empty, then do same as in pkt_l3 *)
  )

  method private pktin_l3 l2pkt = (
    (* - first, enqueue packet. *)
    let enqueued = Pkt_queue.push l2pkt out_queue in
    if not enqueued then (
      s#log_error (lazy "Packet dropped, no room in outgoing queue.");
      drops <- drops + 1
    );

    match state with
      | IDLE -> 
	  (* Control rule #1. 
	     - Set random timer and go to CONTEND. *)

	  (* Invariant 1. *)
	  if Pkt_queue.length out_queue <> 1 then 
	    failwith ("Mac_MACA.pkt_l3: Queue length"
	    ^ (i2s (Pkt_queue.length out_queue)));
	  assert (timer_handle = None);
	  let t = failwith "delay" in 
	  timer_handle <- Some ((Sched.s())#sched_in_handle ~f:s#timer_expire ~t);
	  state <- CONTEND
      | WFDAT | WFCTS | CONTEND | QUIET -> ()
  )

  (* 
     possible timers:
     - when hear a RTS or CTS not for us, timer long enough to for next part of
     'transaction' to complete (we are in QUIET)
     - when want to send a packet, before sending RTS (CONTEND).
     - after sending RTS for us, timer to get CTS back (WFCTS)
     - after sending CTS for us, timer to get DATA back (WFDATA)
    
     Therefore, it should not be possible to have a timer expire when we are in
     IDLE state.
  *)
  method private timer_expire () = (
    
    timer_handle <- None;
    next_timeout <- None;
    match state with 
      | CONTEND -> 
	  (* timeout rule #1 
	     - transmit RTS for destination
	     - go to WFCTS
	  *)
	  assert (Pkt_queue.length out_queue >= 1);
	  (* what happens if we're already sending here??? *)


      | IDLE -> failwith "Mac_MACA.timer_expire when IDLE"
      | WFDAT | WFCTS | QUIET -> () 
	  (* timeout rule #2
	     - go to IDLE
	     - if any packets waiting, do Control rule #1 
	  *)
  )

  method private backend_xmit_complete = ()
    
  method xmit l2pkt = 
    s#pktin_l3 l2pkt; 
    failwith "backend_xmit_complete must be scheduled for after transmission" 
      
  method private backend_stats = 
    let module C = Contention_frontend in 
    {
      colls=
      {
	C.collsRXRX = collsRXRX; 
	C.collsRXTX = collsRXTX;
	C.dropsTXTX = dropsTXTX; 
	C.dropsTXRX = dropsTXRX
      };
      cts_RX=cts_RX;
      cts_TX=cts_TX;
      rts_RX=rts_RX;
      rts_TX=rts_TX;
      drops=drops
    }
end
  
module C = Contention_frontend

let string_of_ostats_colls s = 
  Printf.sprintf 
    "%d RX/RX colls, %d RX/TX colls, %d TX/TX drops, %d TX/RX drops"
    s.colls.C.collsRXRX
    s.colls.C.collsRXTX 
    s.colls.C.dropsTXTX 
    s.colls.C.dropsTXRX 

let string_of_ostats_pkts s = 
  Printf.sprintf 
    "%d CTS RX, %d CTS TX, %d RTS RX, %d RTS TX, %d drops"
    s.cts_RX
    s.cts_TX
    s.rts_RX
    s.rts_TX
    s.drops


let add_ostats s1 s2 =  
  let s1colls = s1.colls and s2colls = s2.colls in 
  let colls_added = 
  {
    C.collsRXRX = s1colls.C.collsRXRX + s2colls.C.collsRXRX;
    C.collsRXTX = s1colls.C.collsRXTX + s2colls.C.collsRXTX;
    C.dropsTXTX = s1colls.C.dropsTXTX + s2colls.C.dropsTXTX;
    C.dropsTXRX = s1colls.C.dropsTXRX + s2colls.C.dropsTXRX
  } in
  {
    colls=colls_added;
    cts_RX=s1.cts_RX + s2.cts_RX;
    cts_TX=s1.cts_TX + s2.cts_TX;
    rts_RX=s1.rts_RX + s2.rts_RX;
    rts_TX=s1.rts_TX + s2.rts_TX;
    drops=s1.drops + s2.drops
  }

let zero_ostats () = 
  {colls=
    {
      C.collsRXRX = 0;
      C.collsRXTX = 0;
      C.dropsTXTX = 0;
      C.dropsTXRX = 0
    };
    cts_RX=0;
    cts_TX=0;
    rts_RX=0;
    rts_TX=0;
    drops=0
  }
